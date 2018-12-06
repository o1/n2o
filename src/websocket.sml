structure WebSocket = struct

datatype FrameType = ContFrame
                   | TextFrame
                   | BinFrame
                   | CloseFrame
                   | PingFrame
                   | PongFrame

type Frame = { fin : bool,
               rsv1 : bool, rsv2 : bool, rsv3 : bool,
               typ : FrameType,
               payload : Word8Vector.vector }

infix 5 &
fun a & b = Word8.andb (a, b)

fun getOctets sock n = Socket.recvVec (sock, n)
fun getWord8 sock = Word8Vector.sub (Socket.recvVec (sock, 1), 0)
fun getWord16be sock = Compat.extract_w16be (getOctets sock 2)
fun getWord64be sock = Compat.extract_w64be (getOctets sock 8)

fun checkCtrlFrame (len : Word64.word) fin =
    if not fin then raise (Fail "Control frames must not be fragmented")
    else if len > 0w125 then raise (Fail "Control frames must not carry payload > 125 bytes")
    else ()

fun unmask key encoded =
    Word8Vector.mapi (fn (i,el) => Word8.xorb (el, Word8Vector.sub (key, i mod 4))) encoded

fun sendFrame sock frame =
    raise Fail "not implemented: sendFrame"

fun getFrame sock : Frame =
    let val b0 = getWord8 sock
        val (fin, rsv1) = (b0 & 0wx80 = 0wx80,b0 & 0wx40 = 0wx40)
        val (rsv2, rsv3) = (b0 & 0wx20 = 0wx20, b0 & 0w10 = 0wx10)
        val opcode = b0 & 0wxF
        val b1 = getWord8 sock
        val mask : bool = b1 & 0wx80 = 0wx80
        val lenflag = b1 & 0wx7F
        val len = case lenflag of 0w126 => getWord16be sock
                                | 0w127 => getWord64be sock
                                | _     => Compat.w8_to_w64 lenflag

        val ft = case opcode of 0wx0 => ContFrame
                              | 0wx1 => TextFrame
                              | 0wx2 => BinFrame
                              | 0wx8 => (checkCtrlFrame len fin; CloseFrame)
                              | 0wx9 => (checkCtrlFrame len fin; PingFrame)
                              | 0wxA => (checkCtrlFrame len fin; PongFrame)
                              | _ => raise Fail ("Unknown opcode: 0x" ^
                                                (Word8.fmt StringCvt.HEX opcode))
        val mask = getOctets sock 4
        val payload = unmask mask (getOctets sock (Word64.toInt len))
     in { fin = fin, rsv1 = rsv1, rsv2 = rsv2, rsv3 = rsv3, typ = ft, payload = payload} end

fun serve sock =
    let val _ = print "serving ws..\n"
        val frame = getFrame sock
     in print "got frame\n";
        serve sock
    end
end
