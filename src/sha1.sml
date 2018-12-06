structure SHA1 = struct

type w32 = Word32.word
type hw32 = w32*w32*w32*w32*w32

structure V = Word8Vector
structure VS = Word8VectorSlice
structure A = Word8Array
structure W64 = Word64

val hinit : hw32 = (0wx67452301,0wxefcdab89,0wx98badcfe,0wx10325476,0wxc3d2e1f0)
val xorb = Word32.xorb
infixr 5 xorb

fun pad bs =
    let open W64
        val len    = V.length bs
        val bitlen = W64.*(W64.fromInt len,0w8)
        val lstbl = bitlen mod 0w512
        val addlen = if lstbl < 0w448 then ((0w448 - lstbl) div 0w8) + 0w8
                     else ((0w512 - lstbl) div 0w8) + 0w64
        val totlen = Int.+(len, toInt addlen)
        val arr = A.array (totlen, 0w0)
    in A.copyVec {src = bs, dst = arr, di = 0};
       A.update (arr, len, 0wx80);
       Compat.pack_w64be (arr, Int.-(totlen,8), bitlen);
       A.vector arr end

local open Word32
      infix orb xorb andb << >>
   in fun lrot(x,n)  = (x << n) orb (x >> Word.-(0w32,n))
      fun ch  (b,c,d) = (b andb c) orb ((notb b) andb d)
      fun par (b,c,d) = b xorb c xorb d
      fun maj (b,c,d) = (b andb c) orb (b andb d) orb (c andb d)
end

fun f (t,b,c,d) =
         if (00 <= t) andalso (t <= 19) then ch(b,c,d)
    else if (20 <= t) andalso (t <= 39) then par(b,c,d)
    else if (40 <= t) andalso (t <= 59) then maj(b,c,d)
    else if (60 <= t) andalso (t <= 79) then par(b,c,d)
    else raise Fail "'t' is out of range"

fun k (t) : w32 =
         if (00 <= t) andalso (t <= 19) then 0wx5a827999
    else if (20 <= t) andalso (t <= 39) then 0wx6ed9eba1
    else if (40 <= t) andalso (t <= 59) then 0wx8f1bbcdc
    else if (60 <= t) andalso (t <= 79) then 0wxca62c1d6
    else raise Fail "'t' is out of range"

fun m bs i t : w32 =
    let val block = VS.slice (bs, 64*i + 4*t, SOME 4)
        val subv = VS.vector block
     in Word32.fromLarge (PackWord32Big.subVec (subv, 0)) end

fun inc x = x + 1

fun w bs i t =
    let val w' = w bs i
     in if (0 <= t) andalso (t <= 15)
        then m bs i t
        else if (16 <= t) andalso (t <= 79)
        then lrot(w'(t-3) xorb w'(t-8) xorb w'(t-14) xorb w'(t-16), 0w1)
        else raise Fail "t is out of range"
    end

fun loop_t wt t (h as (a,b,c,d,e)) =
    if (t = 80) then h
    else let open Word32
         val tmp = lrot(a,0w5) + f(t,b,c,d) + e + k(t) + wt(t)
      in loop_t wt (inc t) (tmp,a,lrot(b,0w30),c,d) end

fun loop_i bs i (res as (h0,h1,h2,h3,h4)) =
    if i = (V.length padded) div 64 then res
    else let val wt = w bs i
             val (a,b,c,d,e) = loop_t wt 0 (h0,h1,h2,h3,h4)
         in loop_i bs (inc i) (h0+a,h1+b,h2+c,h3+d,h4+e) end

fun encode bs =
    let val (h0,h1,h2,h3,h4) = loop_i (pad bs) 0 hinit
        val arr = A.array (20,0w0)
        fun pack i x = PackWord32Big.update (arr,i,Word32.toLarge x)
    in pack 0 h0; pack 1 h1; pack 2 h2; pack 3 h3; pack 4 h4;
       A.vector arr end
end

structure SHA1Test = struct
structure V = Word8Vector
fun hexstr (vec:V.vector):string =
    V.foldr (fn (e,a) => (if (Word8.<= (e, 0wxf))
             then "0" else "") ^ (Word8.toString e) ^ a) "" vec
fun hex v = String.map Char.toLower (hexstr v)
fun test (x, expected) = let
    open LargeWord
    open SHA1
    infix <~
    val raw = Byte.stringToBytes x
    val actual = hex (encode raw)
 in if expected = actual then ()
    else raise Fail ("\nExpected: " ^ expected  ^ "\n  actual: " ^ actual ^ "\n")
end
end

val _ = (SHA1Test.test("abcdef", "1f8ac10f23c5b5bc1167bda84b833e5c057a77d2"))
