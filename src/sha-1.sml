type bytestr = Word8Vector.vector

structure Sha1
          (* :> sig *)
          (*     val encode : bytestr -> bytestr *)
          (* end *)
= struct

structure V = Word8Vector
structure A = Word8Array
structure LW = LargeWord
type hashf   = word*word*word -> word
infixr 3 /> fun f /> y = fn x => f (x, y)

val a = 0wx67452301
val b = 0wxEFCDAB89
val c = 0wx98BADCFE
val d = 0wx10325476
val e = 0wxC3D2E1F0

local
    open Word32
    infix orb xorb andb << >>
in
    fun rotl (b,n)  = (b << n) orb (b >> (Word.- (0w32,n)))
    fun ch  (x,y,z) = z orb (x andb (y orb z))
    fun par (x,y,z) = x xorb y xorb z
    fun maj (x,y,z) = (x andb y) xorb (x andb z) xorb (y andb z)
end

val lwlen = LW.fromInt o V.length
val bitlen = (LW.* /> 0w8) o lwlen
fun const x = fn _ => x

fun fk t =
    if      (0w00 <= t) andalso (t <= 0w19) then (ch,  0wx5A827999)
    else if (0w20 <= t) andalso (t <= 0w39) then (par, 0wx6ED9EBA1)
    else if (0w40 <= t) andalso (t <= 0w59) then (maj, 0wx8F1BBCDC)
    else if (0w60 <= t) andalso (t <= 0w79) then (par, 0wxCA62C1D6)
    else raise Fail "'t' is out of range"

(* fun zbits bs = *)
(*     let val lbsz = (bitlen bs) mod 0w512 *)
(*     in if lbsz < 0w448 then 0w447 - lbsz *)
(*        else 0w448 + (0w511 - lbsz) end *)

fun preproc (bs : V.vector) : V.vector =
    let
        open LW
        val len = lwlen bs
        val lbsz = (bitlen bs) mod 0w512
        val bn = ((0w448 - lbsz) div 0w8) - 0w1
        val arr = A.array (8, 0w0)
        val () = PackWord64Big.update (arr,0,len)
        (* val nv = V.tabulate (LW.toInt ((zbits bs) mod 0w8), const 0w0) *)
    in
        if lbsz < 0w448 then
            V.concat [bs, V.fromList [0wx80],
                      V.tabulate (toInt bn, const 0w0)
                      ,A.vector arr
                     ]
        else raise Fail ""
    end

fun encode (bs : bytestr) : bytestr =
    raise Fail "unimplemented!"

end

structure V = Word8Vector
structure A = Word8Array
val _ = let
    open LargeWord
    open UTF8
    val v = V.fromList [0wx61,0wx62,0wx63]
    val lbsz = (Sha1.bitlen v) mod 0w512
    val bn = ((0w448 - lbsz) div 0w8)
    val res = Sha1.preproc v
in
    print (V.foldl (fn (e,a) => (Word8.toString e) ^ a) "" (A.concat [A.fromList[0w1,0w2,0w3]]));
    print "\n"
end


