structure WebSocket = struct

structure W8 = Word8
structure W64 = Word64
structure V = Word8Vector
structure A = Word8Array
structure AS = Word8ArraySlice

datatype FrameType = Cont | Text | Bin | Close | Ping | Pong

datatype Msg = TextMsg of V.vector (*TODO:utf8*)
             | BinMsg of V.vector
             | CloseMsg of Word32.word
             | ContMsg of V.vector
             | PingMsg
             | PongMsg

datatype Res = Error of string
             | Reply of Msg
             | Ok

type Frame = { fin : bool,
               rsv1 : bool, rsv2 : bool, rsv3 : bool,
               typ : FrameType, payload : V.vector }

fun bytes sock n = Socket.recvVec (sock, n)
fun w8  sock = V.sub (bytes sock 1, 0)
fun w16 sock = Compat.extract_w16be (bytes sock 2)
fun w64 sock = Compat.extract_w64be (bytes sock 8)

fun check (len : W64.word) fin =
    if not fin then raise (Fail "Control frames must not be fragmented")
    else if len > 0w125 then raise (Fail "Control frames must not carry payload > 125 bytes")
    else ()

fun unmask (key,encoded) =
    V.mapi (fn (i,el) => W8.xorb(el,V.sub(key,i mod 4))) encoded

fun opcode Text : Word8.word = 0wx1
  | opcode Bin               = 0wx2
  | opcode Cont              = 0wx0
  | opcode Close             = 0wx8
  | opcode Ping              = 0wx9
  | opcode Pong              = 0wxA

fun sndf sock ({fin,typ,payload,...} : Frame) : unit =
    let val len = V.length payload
        val (b1,di,pack) =      if len < 126   then (W8.fromInt(len),2,fn _ => ())
                           else if len < 65535 then (0w126,4,Compat.pack_w16be)
                           else                     (0w127,10,Compat.pack_w64be)
        val arr = A.array (len+di,0w0)
    in A.update(arr,0,W8.orb(0wx80,opcode typ));
       A.update(arr,1,b1);
       pack(arr,2,W64.fromInt(len));
       A.copyVec {src=payload,dst=arr,di=di};
       Socket.sendArr(sock,AS.full(arr));
       ()
    end

fun fr t b : Frame = {fin=true,rsv1=false,rsv2=false,rsv3=false,typ=t,payload=b}
val emp : V.vector = V.fromList[]

fun send sock msg =
    case msg of
        (TextMsg b)   => sndf sock (fr Text b)(*TODO check length*)
      | (BinMsg b)   => sndf sock (fr Bin b)(*TODO check length*)
      | (CloseMsg b) => let val arr = A.array(2,0w0)
                        in PackWord16Big.update(arr,0,Word32.toLarge b);
                           sndf sock (fr Close (A.vector arr)) end
      | PingMsg => sndf sock (fr Ping emp)
      | PongMsg => sndf sock (fr Pong emp)
      | _ => raise Fail "cont msg!"

fun parse sock : Frame =
    let val b0 = w8 sock
        val (fin, rsv1) = (W8.andb(b0,0wx80) = 0wx80, W8.andb(b0,0wx40) = 0wx40)
        val (rsv2, rsv3) = (W8.andb(b0,0wx20) = 0wx20, W8.andb(b0,0w10) = 0wx10)
        val opcode = W8.andb(b0,0wxF)
        val b1 = w8 sock
        val lenf = W8.andb(b1,0wx7F)
        val len = case lenf of
                      0w126 => w16 sock
                    | 0w127 => w64 sock
                    | _     => Compat.w8_to_w64 lenf
        val ft = case opcode of
                     0wx0 => Cont
                   | 0wx1 => Text
                   | 0wx2 => Bin
                   | 0wx8 => (check len fin; Close)
                   | 0wx9 => (check len fin; Ping)
                   | 0wxA => (check len fin; Pong)
                   | _ => raise Fail ("Unknown opcode: 0x" ^ (Word8.fmt StringCvt.HEX opcode))
        val (mask,masker) = if W8.andb(b1,0wx80)=0wx80 then (bytes sock 4,unmask)
                            else (emp,fn (_,b) => b)
        val payload = unmask (mask,(bytes sock (W64.toInt len)))
    in { fin = fin, rsv1 = rsv1, rsv2 = rsv2, rsv3 = rsv3, typ = ft, payload = payload} end

fun recv sock : Msg =
    case (parse sock) of
        {typ=Close,payload,...} => CloseMsg (Word32.fromLarge(PackWord16Big.subVec(payload,0)))
      | {typ=Ping,...} => PingMsg
      | {typ=Pong,...} => PongMsg
      | {typ=Text,payload,...} => TextMsg payload
      | {typ=Bin,payload,...} => BinMsg payload
      | {typ=Cont,payload,...} => ContMsg payload

fun serve sock (hnd : Msg -> Res) =
    let val msg = recv sock
    in (case msg of
           PongMsg => ()
         | PingMsg => send sock PongMsg
         | (CloseMsg b) => print ("Received close message: code "^(Word32.fmt StringCvt.DEC b)^"\n")
         | ContMsg _ => raise Fail "cont frame"
         | msg' =>
           case hnd msg' of
                Error err => (print err; print "\n"; Socket.close sock)
              | Reply msg => send sock msg
              | _ => ());
       serve sock hnd
    end
end
