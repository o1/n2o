structure WebSocket = struct

structure W8 = Word8
structure W64 = Word64
structure V = Word8Vector
structure A = Word8Array
structure AS = Word8ArraySlice

datatype FrameType = ContFrame
                   | TextFrame
                   | BinFrame
                   | CloseFrame
                   | PingFrame
                   | PongFrame

type Frame = { fin : bool,
               rsv1 : bool, rsv2 : bool, rsv3 : bool,
               typ : FrameType, payload : V.vector }

datatype Msg = TxtMsg of V.vector (*TODO:utf8*)
             | BinMsg of V.vector
             | ClsMsg

datatype Res = Error of string
       | Reply of Msg
       | Ok

fun bytes sock n = Socket.recvVec (sock, n)
fun w8 sock = V.sub (Socket.recvVec (sock, 1), 0)
fun w16 sock = Compat.extract_w16be (bytes sock 2)
fun w64 sock = Compat.extract_w64be (bytes sock 8)

fun check (len : W64.word) fin =
    if not fin then raise (Fail "Control frames must not be fragmented")
    else if len > 0w125 then raise (Fail "Control frames must not carry payload > 125 bytes")
    else ()

fun unmask key encoded =
    V.mapi (fn (i,el) => W8.xorb(el,V.sub(key,i mod 4))) encoded

fun opcode (TxtMsg _) : W8.word = 0wx1
  | opcode (BinMsg _)           = 0wx2
  | opcode _ = raise (Fail "unsupported message 1")

fun body (TxtMsg x) = x
  | body (BinMsg x) = x
  | body _ = raise (Fail "unsupported message 2")

fun send sock (msg : Msg) : unit =
    let val payload = body msg
        val len = V.length payload
        val arr = A.array (len+2,0w0)
    in (if (len > 127) orelse (len < 0) then raise (Fail "0 < len msg < 127") else ());
       A.update(arr,0,W8.orb(0wx80,opcode msg));
       A.update(arr,1,W8.fromInt len);
       A.copyVec {src=payload,dst=arr,di=2};
       Socket.sendArr(sock,AS.full(arr));
       ()
    end

fun parse sock : Frame =
    let val b0 = w8 sock
        val (fin, rsv1) = (W8.andb(b0,0wx80) = 0wx80, W8.andb(b0,0wx40) = 0wx40)
        val (rsv2, rsv3) = (W8.andb(b0,0wx20) = 0wx20, W8.andb(b0,0w10) = 0wx10)
        val opcode = W8.andb(b0,0wxF)
        val b1 = w8 sock
        val mask : bool = W8.andb(b1,0wx80) = 0wx80
        val lenflag = W8.andb(b1,0wx7F)
        val len = case lenflag of 0w126 => w16 sock
                                | 0w127 => w64 sock
                                | _     => Compat.w8_to_w64 lenflag

        val ft = case opcode of 0wx0 => ContFrame
                              | 0wx1 => TextFrame
                              | 0wx2 => BinFrame
                              | 0wx8 => (check len fin; CloseFrame)
                              | 0wx9 => (check len fin; PingFrame)
                              | 0wxA => (check len fin; PongFrame)
                              | _ => raise Fail ("Unknown opcode: 0x" ^
                                                 (Word8.fmt StringCvt.HEX opcode))
        val mask = bytes sock 4
        val payload = unmask mask (bytes sock (W64.toInt len))
    in { fin = fin, rsv1 = rsv1, rsv2 = rsv2, rsv3 = rsv3, typ = ft, payload = payload} end

fun recv sock : Msg =
    case (parse sock) of
        {typ=CloseFrame,...}                 => ClsMsg
      | {typ=BinFrame,payload=payload,...}  => BinMsg payload
      | {typ=TextFrame,payload=payload,...} => TxtMsg payload
      | _ => raise (Fail "usupported message 0")

fun echo (msg : Msg) : Res =
    (case msg of
         ClsMsg => (print "Received close message\n"; Ok)
       | _ => Reply msg)
    handle (Fail err) => Error err

fun serve sock =
    let (*val _ = print "serving ws..\n"*)
        val msg = recv sock
    in (*print "got msg\n";*)
       (case echo msg of
            Error err => (print err; print "\n"; Socket.close sock)
          | Reply msg => send sock msg
          | _ => ());
       serve sock
    end
end
