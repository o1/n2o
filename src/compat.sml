structure Compat = struct

structure A = Word8Array
structure V = Word8Vector
structure W8 = Word8
structure W64 = Word64

val w8_to_w64 =
    if LargeWord.wordSize = 64
    then W64.fromLarge o W8.toLarge
    else W64.fromLargeInt o W8.toLargeInt

val w64_to_w8 =
    if LargeWord.wordSize = 64
    then W8.fromLarge o W64.toLarge
    else W8.fromLargeInt o W64.toLargeInt

local
    val orb = W64.orb
    val (op-) = Int.-
    val (op+) = Int.+
    infix orb + -
in

fun pack_w16be (arr,i,w) =
    (A.update (arr, i,   w64_to_w8(Word64.>>(w,0w8)));
     A.update (arr, i+1, w64_to_w8(w)))

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

structure CompatTest = struct

fun test() =
    let val exp = Word8Vector.fromList[0wxAA,0wxBB]
        val arr = Word8Array.array(2,0w0);
        fun fmt v i = Word8.toString (Word8Vector.sub(v,i))
        val _ = Compat.pack_w16be(arr,0,0wxAABB)
        val act = Word8Array.vector arr
    in if exp = act then ()
       else raise Fail ("Expected: AABB\nActual: " ^ (fmt act 0) ^ (fmt act 1))
    end

end

val _ = CompatTest.test()
