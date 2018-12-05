structure Compat = struct

structure A = Word8Array
structure V = Word8Vector
structure W8 = Word8
structure W64 = Word64

fun w8_to_w64 w =
    if LargeWord.wordSize = 64
    then W64.fromLarge (W8.toLarge w)
    else W64.fromLargeInt (W8.toLargeInt w)

fun w64_to_w8 w =
    if LargeWord.wordSize = 64
    then W8.fromLarge (W64.toLarge w)
    else W8.fromLargeInt (W64.toLargeInt w)

local
    val orb = W64.orb
    val (op-) = Int.-
    val (op+) = Int.+
    infix orb + -
in
fun pack_w64be (arr,i,w) =
    (A.update (arr, i,   w64_to_w8(Word64.>>(w,0w56)));
     A.update (arr, i+1, w64_to_w8(Word64.>>(w,0w48)));
     A.update (arr, i+2, w64_to_w8(Word64.>>(w,0w40)));
     A.update (arr, i+3, w64_to_w8(Word64.>>(w,0w32)));
     A.update (arr, i+4, w64_to_w8(Word64.>>(w,0w24)));
     A.update (arr, i+5, w64_to_w8(Word64.>>(w,0w16)));
     A.update (arr, i+6, w64_to_w8(Word64.>>(w,0w8)));
     A.update (arr, i+7, w64_to_w8(w)))

fun extract_w16be vec =
    let
        val p0 = W64.<<(w8_to_w64(V.sub(vec,0)),0w8)
        val p1 =        w8_to_w64(V.sub(vec,1))
    in
        p0 orb p1
    end
fun extract_w64be vec =
    let
        val p0 = W64.<<(w8_to_w64(V.sub(vec,0)),0w56)
        val p1 = W64.<<(w8_to_w64(V.sub(vec,1)),0w48)
        val p2 = W64.<<(w8_to_w64(V.sub(vec,2)),0w40)
        val p3 = W64.<<(w8_to_w64(V.sub(vec,3)),0w32)
        val p4 = W64.<<(w8_to_w64(V.sub(vec,4)),0w24)
        val p5 = W64.<<(w8_to_w64(V.sub(vec,5)),0w16)
        val p6 = W64.<<(w8_to_w64(V.sub(vec,6)),0w8)
        val p7 =        w8_to_w64(V.sub(vec,7))
    in
        p0 orb p1 orb p2 orb p3 orb p4 orb p5 orb p6 orb p7
    end
end
end
