structure WebSocket = struct

structure W8 = Word8
structure W64 = Word64
structure V = Word8Vector

datatype FrameType = ContFrame
                   | TextFrame
                   | BinFrame
                   | CloseFrame
                   | PingFrame
                   | PongFrame

type Frame = { fin : bool,
               rsv1 : bool, rsv2 : bool, rsv3 : bool,
               typ : FrameType,payload : V.vector }

fun bytes sock n = Socket.recvVec (sock, n)
fun w8 sock = V.sub (Socket.recvVec (sock, 1), 0)
fun w16 sock = Compat.extract_w16be (bytes sock 2)
fun w64 sock = Compat.extract_w64be (bytes sock 8)

fun check (len : W64.word) fin =
    if not fin then raise (Fail "Control frames must not be fragmented")
    else if len > 0w125 then raise (Fail "Control frames must not carry payload > 125 bytes")
    else ()

fun unmask key encoded =
    V.mapi (fn (i,el) => W8.xorb (el, V.sub (key, i mod 4))) encoded

fun send sock frame =
    raise Fail "not implemented: sendFrame"

fun recv sock : Frame =
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

fun serve sock =
    let val _ = print "serving ws..\n"
        val frame = recv sock
     in print "got frame\n";
        serve sock
    end
end
