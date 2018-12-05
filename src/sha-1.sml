type bytestr = Word8Vector.vector

structure Sha1
          (* :> sig *)
          (*     val encode : bytestr -> bytestr *)
          (* end *)
= struct

structure V = Word8Vector
structure VS = Word8VectorSlice
structure A = Word8Array
structure LW = LargeWord
val xorb = Word32.xorb
infixr 5 xorb
infixr 3 /> fun f /> y = fn x => f (x, y)

val tobitlen = (LW.* /> 0w8) o LW.fromInt
fun const x = fn _ => x

fun preproc (bs : V.vector) : V.vector =
    let
        open LW
        val len = V.length bs
        val bitlen = LW.mod ((tobitlen len), 0w512)
        val addlen = if bitlen < 0w448 then ((0w448 - bitlen) div 0w8) + 0w8
                     else ((0w512 - bitlen) div 0w8) + 0w512
        val totlen = (fromInt len) + addlen
        val arr = A.array (toInt totlen, 0w0)
    in
        A.copyVec {src = bs, dst = arr, di = 0};
        A.update (arr,len,0wx80);
        PackWord64Big.update (arr,toInt((totlen - 0w8) div 0w8),tobitlen len);
        A.vector arr
    end

val hinit = (0wx67452301,0wxefcdab89,0wx98badcfe,0wx10325476,0wxc3d2e1f0)

infix <~
local
    open Word32
    infix orb xorb andb << >> -
    val (op-) = Word.-
in
fun x <~ n  = (x << n) orb (x >> (0w32 - n))
fun ch  (b,c,d) = (b andb c) orb ((notb b) andb d)
fun par (b,c,d) = b xorb c xorb d
fun maj (b,c,d) = (b andb c) orb (b andb d) orb (c andb d)
end


fun f (t,b,c,d) =
    if      (00 <= t) andalso (t <= 19) then ch(b,c,d)
    else if (20 <= t) andalso (t <= 39) then par(b,c,d)
    else if (40 <= t) andalso (t <= 59) then maj(b,c,d)
    else if (60 <= t) andalso (t <= 79) then par(b,c,d)
    else raise Fail "'t' is out of range"
fun k (t) =
    if      (00 <= t) andalso (t <= 19) then 0wx5a827999
    else if (20 <= t) andalso (t <= 39) then 0wx6ed9eba1
    else if (40 <= t) andalso (t <= 59) then 0wx8f1bbcdc
    else if (60 <= t) andalso (t <= 79) then 0wxca62c1d6
    else raise Fail "'t' is out of range"

fun m bs i t : Word32.word =
    let
        val block = VS.slice (bs, 64*i + 4*t, SOME 4)
        val subv = VS.vector block
    in
        Word32.fromLarge (PackWord32Big.subVec (subv, 0))
    end

(* fun w bs i t = if (0 <= t) andalso (t <= 15) *)
(*                then m bs i t *)
(*                else if (16 <= t) andalso (t <= 79) *)
(*                then rotl ((w bs i (t-3)) xorb (w bs i (t-8)) xorb (w bs i (t-14)) xorb (w bs i (t-16)), 0w1) *)
(*                else raise Fail "t is out of range" *)

fun inc x = x + 1
fun loop_i bs i (h as (h0,h1,h2,h3,h4)) =
    if i = (((V.length bs) div 64) - 1) then h
    else
        let
            fun wt t =
                if (0 <= t) andalso (t <= 15)
                then m bs i t
                else if (16 <= t) andalso (t <= 79)
                then (wt(t-3) xorb wt(t-8) xorb wt(t-14) xorb wt(t-16)) <~ 0w1
                else raise Fail "t is out of range"
            fun loop_t t (h as (a,b,c,d,e)) =
                if (t = 80) then h
                else
                    let
                        open Word32
                        val tmp = (a <~ 0w5) + f(t,b,c,d) + e + k(t) + wt(t)
                    in
                        (* print (Int.toString i); print ":"; *)
                        (* print (Int.toString t);print "; "; *)
                        (* print (Word32.toString (wt t)); print "; "; *)
                        loop_t (inc t) (tmp,a,b <~ 0w30,c,d)
                    end
            val (a,b,c,d,e) = loop_t 0 (h0,h1,h2,h3,h4)
        in
            loop_i bs (inc i) (h0+a,h1+b,h2+c,h3+d,h4+e)
        end

fun encode bs =
    let
        val (h0,h1,h2,h3,h4) = loop_i (preproc bs) 0 hinit
        val arr = A.array (20,0w0)
        fun pack i x = PackWord32Big.update (arr,i,Word32.toLarge x)
    in
        Word32Vector.appi (fn (i,h) => ignore (pack i h)) (Word32Vector.fromList[h0,h1,h2,h3,h4]);
        A.vector arr
    end
end

structure V = Word8Vector
structure A = Word8Array
val _ = let
    open LargeWord
    open Sha1
    infix <~
    fun hexstr (vec:V.vector):string =
        V.foldr (fn (e,a) => (if (Word8.<= (e, 0wxf)) then "0" else "") ^ (Word8.toString e) ^ a) "" vec
    fun hex v = String.map Char.toLower (hexstr v)
    (* val prep = Sha1.preproc (Byte.stringToBytes "abcdefghijklmnopqrstuvwxyz") *)
    val prep = preproc (Byte.stringToBytes "abcde")
    val _ = print (hex prep ^ "\n")
    val enc = encode prep
in
    print "\n                 00000000000000000000000000000000\n";
    print "\nbefore rotation: 000000000";
    print (Word32.fmt StringCvt.BIN 0wx616263);
    print "\nafter rotation:\n";
    print "\n1                00000000";
    print (Word32.fmt StringCvt.BIN (0wx616263<~0w1));
    print "\n2                0000000";
    print (Word32.fmt StringCvt.BIN (0wx616263<~0w2));
    print "\n3                000000";
    print (Word32.fmt StringCvt.BIN (0wx616263<~0w3));
    print "\n4                00000";
    print (Word32.fmt StringCvt.BIN (0wx616263<~0w4));
    print "\n5                0000";
    print (Word32.fmt StringCvt.BIN (0wx616263<~0w5));
    print "\n6                000";
    print (Word32.fmt StringCvt.BIN (0wx616263<~0w6));
    print "\n7                00";
    print (Word32.fmt StringCvt.BIN (0wx616263<~0w7));
    print "\n8                0";
    print (Word32.fmt StringCvt.BIN (0wx616263<~0w8));
    print "\n9                ";
    print (Word32.fmt StringCvt.BIN (0wx616263<~0w9));
    print "\n10               ";
    print (Word32.fmt StringCvt.BIN (0wx616263<~0w10));
    print "\n11               0000";
    print (Word32.fmt StringCvt.BIN (0wx616263<~0w11));
    (* print (Word32.toString (Sha1.m prep 0 6)); *)
    print "\nenc:\n";
    print (hex enc);
    print "\n----\n";
    print ""
end
