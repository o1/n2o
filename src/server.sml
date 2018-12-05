structure WebSocket = struct
datatype FrameType = ContFrame | TextFrame | BinFrame | CloseFrame | PingFrame | PongFrame
type Frame = { fin : bool, rsv1 : bool, rsv2 : bool, rsv3 : bool,
               typ : FrameType, payload : Word8Vector.vector }
infix 5 &
fun a & b = Word8.andb (a, b)

fun getOctets sock n = Socket.recvVec (sock, n)
fun getWord8 sock = Word8Vector.sub (Socket.recvVec (sock, 1), 0)
fun getWord16be sock = PackWord16Big.subVec (Socket.recvVec (sock, 2), 0)
fun getWord64be cursor = raise Fail "64-bit wide words"

fun checkCtrlFrame (len : LargeWord.word) fin =
    if not fin then raise (Fail "Control frames must not be fragmented")
    else if len > 0w125 then raise (Fail "Control frames must not carry payload > 125 bytes")
    else ()

fun unmask key encoded =
    Word8Vector.mapi (fn (i,el) => Word8.xorb (el, Word8Vector.sub (key, i mod 4))) encoded

fun getFrame sock : Frame =
    let
        val _ = print "step0\n"
        val b0 = getWord8 sock
        val (fin, rsv1) = (b0 & 0wx80 = 0wx80,b0 & 0wx40 = 0wx40)
        val (rsv2, rsv3) = (b0 & 0wx20 = 0wx20, b0 & 0w10 = 0wx10)
        val _ = print "step1\n"
        val opcode = b0 & 0wxF
        val b1 = getWord8 sock
        val mask : bool = b1 & 0wx80 = 0wx80
        val lenflag = b1 & 0wx7F
        val len = case lenflag of
                      0w126 => getWord16be sock
                    | 0w127 => getWord64be sock
                    | _     => Word8.toLarge lenflag
        val ft = case opcode of
                     0wx0 => ContFrame
                   | 0wx1 => TextFrame
                   | 0wx2 => BinFrame
                   | 0wx8 => (checkCtrlFrame len fin; CloseFrame)
                   | 0wx9 => (checkCtrlFrame len fin; PingFrame)
                   | 0wxA => (checkCtrlFrame len fin; PongFrame)
                   | _ => raise Fail ("Unknown opcode: 0x" ^ (Word8.fmt StringCvt.HEX opcode))
        val mask = getOctets sock 4
        val payload = unmask mask (getOctets sock (LargeWord.toInt len))
    in
        { fin = fin, rsv1 = rsv1, rsv2 = rsv2, rsv3 = rsv3, typ = ft, payload = payload}
    end

fun serve sock =
    let
        val _ = print "serving ws..\n"
        val frame = getFrame sock
    in
        print "got frame\n";
        serve sock
    end
end

structure Server = struct
type Req = { cmd : string, path : string, headers : (string*string) list, vers : string }
type Resp = { status : int, headers : (string*string) list, body : Word8Vector.vector }
exception BadRequest of string
exception NotFound of string

fun collect mark i sepLen acc slc =
    if i > (mark + sepLen) then
         (Word8VectorSlice.subslice (slc, mark, SOME ((i-mark)-sepLen)))::acc
    else acc

fun recur s l len sepLen mark i [] acc =
    recur s l len sepLen i i l (collect mark i sepLen acc s)
  | recur s l len sepLen mark i (b::bs) acc =
    if i = len then List.rev (collect mark i 0 acc s)
    else recur s l len sepLen mark (i+1)
         (if b = Word8VectorSlice.sub (s, i) then bs else l) acc

fun tokens slc (sep : string) =
    let val lst = map (Word8.fromInt o Char.ord) (String.explode sep)
        val len = Word8VectorSlice.length slc
        val sepLen = String.size sep
    in recur slc lst len sepLen 0 0 lst [] end

val sliceToStr = Byte.bytesToString o Word8VectorSlice.vector
fun tokens' slc (sep : string) = map sliceToStr (tokens slc sep)

fun parseHeaders nil = nil
  | parseHeaders (ln::lns) = (case tokens' ln ": " of
                                  k::v::_ => (k,v)
                                | _ => raise BadRequest "Invalid headers")
                             :: (parseHeaders lns)

fun writeHeaders nil = ""
  | writeHeaders ((k,v)::hs) = k ^ ": " ^ v ^ "\r\n" ^ (writeHeaders hs)

fun parseReq slc : Req =
    case tokens slc "\r\n" of
        nil => raise BadRequest "Malformed HTTP request"
      | lines as (hd::tl) =>
        case tokens' hd " " of
            "GET"::path::vers::_ => { cmd = "GET", path = path, headers = parseHeaders tl, vers = vers }
          | _ => raise BadRequest "Method must be GET"

fun lower str = String.map Char.toLower str
fun header nam (req : Req) = List.find (fn (k,v) => lower k = lower nam) (#headers req)
fun needUpgrade req =
    case header "Upgrade" req of
        SOME (_,v) => (lower v) = "websocket"
      | _ => false
fun getKey req =
    case header "Sec-WebSocket-Key" req of
        NONE => raise BadRequest "No Sec-WebSocket-Key header"
      | SOME (_,key) => let
          val magic = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"
          val k = key^magic
      in
          Base64.encode (Sha1.encode (Byte.stringToBytes k))
      end
fun checkHandshake req =
    (if #cmd req <> "GET" then raise BadRequest "Method must be GET" else ();
     if #vers req <> "HTTP/1.1" then raise BadRequest "HTTP version must be 1.1" else ();
     case header "Sec-WebSocket-Version" req of
         SOME (_,"13") => ()
       | _ => raise BadRequest "WebSocket version must be 13")
fun upgrade sock req =
    (checkHandshake req;
     { status = 101, headers = [("Upgrade", "websocket"),
                                ("Connection", "Upgrade"),
                                ("Sec-WebSocket-Accept", getKey req)],
       body = Word8Vector.fromList nil }
    )

fun sendBytes sock bytes = ignore (Socket.sendVec (sock, Word8VectorSlice.full bytes))
fun sendStr sock str = sendBytes sock (Byte.stringToBytes str)
fun sendList sock lst = sendStr sock (String.concat lst)

fun fileResp filePath =
    let val stream = BinIO.openIn filePath
        val data = BinIO.inputAll stream
        val () = BinIO.closeIn stream
    in { status = 200,
         headers = [("Content-Type", "text/html"),
                    ("Content-Length", Int.toString (Word8Vector.length data))],
         body = data }
    end

fun respCode 101 = "Switching Protocols"
  | respCode 200 = "OK"
  | respCode 400 = "Bad Request"
  | respCode 404 = "Not Found"
  | respCode _ = "Internal Server Error"

fun sendResp sock {status=status,headers=headers,body=body} =
    (sendList sock ["HTTP/1.1 ", Int.toString status, " ", respCode status, "\r\n",
                    writeHeaders headers, "\r\n"];
     sendBytes sock body;
     sendStr sock "\r\n\r\n")

fun sendError sock code body =
    (print body;
     sendResp sock {status=code,headers=[],body=Byte.stringToBytes body};
     Socket.close sock)

fun serve sock : Resp =
    let
        val req = parseReq (Word8VectorSlice.full (Socket.recvVec (sock, 2048)))
        val path = #path req
        val reqPath = case path of
                          "/" => "/index"
                        | p => if String.isPrefix "/ws" p
                               then String.extract (p, 3, NONE)
                               else p
    in
        if needUpgrade req then (print "need upgrade\n"; upgrade sock req)
        else (fileResp ("static/html" ^ reqPath ^ ".html"))
             handle Io => (fileResp (String.extract (path, 1, NONE))) handle Io => raise NotFound path
    end

fun connMain sock =
    (case serve sock of
         resp => (sendResp sock resp; if (#status resp)<>101 then ignore (Socket.close sock)
                                      else WebSocket.serve sock))
    handle BadRequest err => sendError sock 400 ("Bad Request: " ^ err ^ "\n")
         | NotFound path  => sendError sock 404 ("Not Found: " ^ path ^ "\n")

fun acceptLoop server_sock =
    let val (s, _) = Socket.accept server_sock
    in
        print "Accepted a connection.\n";
        CML.spawn (fn () => connMain(s));
        acceptLoop server_sock
    end

fun run (program_name, arglist) =
    let val s = INetSock.TCP.socket()
    in
        Socket.Ctl.setREUSEADDR (s, true);
        Socket.bind(s, INetSock.any 8989);
        Socket.listen(s, 5);
        print "Entering accept loop...\n";
        acceptLoop s
    end

end
