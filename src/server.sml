structure Server = struct

type Req = { cmd : string,
             path : string,
             headers : (string*string) list, vers : string }

type Resp = { status : int,
              headers : (string*string) list,
              body : Word8Vector.vector }

exception BadRequest of string
exception NotFound of string

fun collect mark i sl acc slc =
    if i > (mark + sl)
    then (Word8VectorSlice.subslice (slc, mark, SOME ((i-mark)-sl)))::acc
    else acc

fun recur s l len sl mark i [] acc =
    recur s l len sl i i l (collect mark i sl acc s)
  | recur s l len sl mark i (b::bs) acc =
    if i = len
    then List.rev (collect mark i 0 acc s)
    else recur s l len sl mark (i+1)
         (if b = Word8VectorSlice.sub (s, i) then bs else l) acc

fun tokens slc (sep : string) =
    let val lst = map (Word8.fromInt o Char.ord) (String.explode sep)
        val len = Word8VectorSlice.length slc
     in recur slc lst len (String.size sep) 0 0 lst []
    end

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
       | SOME (_,key) => let val magic = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"
                          in Base64.encode(SHA1.encode(Byte.stringToBytes(key^magic))) end

fun checkHandshake req =
    (if #cmd req <> "GET" then raise BadRequest "Method must be GET" else ();
     if #vers req <> "HTTP/1.1" then raise BadRequest "HTTP version must be 1.1" else ();
     case header "Sec-WebSocket-Version" req of
          SOME (_,"13") => ()
        | _ => raise BadRequest "WebSocket version must be 13")

fun upgrade sock req =
    (checkHandshake req;
     { body = Word8Vector.fromList nil,
       status = 101, headers = [("Upgrade", "websocket"),
                                ("Connection", "Upgrade"),
                                ("Sec-WebSocket-Accept", getKey req)] })

fun sendBytes sock bytes = ignore (Socket.sendVec (sock, Word8VectorSlice.full bytes))
fun sendStr sock str = sendBytes sock (Byte.stringToBytes str)
fun sendList sock lst = sendStr sock (String.concat lst)

fun fileResp filePath =
    let val stream = BinIO.openIn filePath
        val data = BinIO.inputAll stream
        val () = BinIO.closeIn stream
    in { status = 200, body = data,
         headers = [("Content-Type", "text/html"),
                    ("Content-Length", Int.toString (Word8Vector.length data))] }
    end

fun respCode 101 = "Switching Protocols"
  | respCode 200 = "OK"
  | respCode 400 = "Bad Request"
  | respCode 404 = "Not Found"
  | respCode _ = "Internal Server Error"

fun sendResp sock {status=status,headers=headers,body=body} =
    (sendList sock ["HTTP/1.1 ", Int.toString status,
                            " ", respCode status, "\r\n",
                                 writeHeaders headers, "\r\n"];
     sendBytes sock body;
     sendStr sock "\r\n\r\n")

fun sendError sock code body =
    (print body;
     sendResp sock {status=code,headers=[],body=Byte.stringToBytes body};
     Socket.close sock)

fun router path =
    case path of
         "/" => "/index"
         | p => if String.isPrefix "/ws" p
                then String.extract (p, 3, NONE)
                else p

fun serve sock : Resp =
    let val req = parseReq (Word8VectorSlice.full (Socket.recvVec (sock, 2048)))
        val path = #path req
        val reqPath = router path
     in if needUpgrade req
        then (print "need upgrade\n"; upgrade sock req)
        else (fileResp ("static/html" ^ reqPath ^ ".html"))
             handle Io => (fileResp (String.extract (path, 1, NONE)))
             handle Io => raise NotFound path
    end

fun switch sock =
    case serve sock of
         resp => (sendResp sock resp;
                  if (#status resp)<>101 then ignore (Socket.close sock)
                                         else WebSocket.serve sock)
fun connMain sock =
    switch sock
    handle BadRequest err => sendError sock 400 ("Bad Request: " ^ err ^ "\n")
         | NotFound path  => sendError sock 404 ("Not Found: " ^ path ^ "\n")

fun acceptLoop server_sock =
    let val (s, _) = Socket.accept server_sock
     in print "Accepted a connection.\n";
        CML.spawn (fn () => connMain(s));
        acceptLoop server_sock
    end

fun run (program_name, arglist) =
    let val s = INetSock.TCP.socket()
     in Socket.Ctl.setREUSEADDR (s, true);
        Socket.bind(s, INetSock.any 8989);
        Socket.listen(s, 5);
        print "Entering accept loop...\n";
        acceptLoop s
    end
end
