(* UTF-8 Encoder/Decoder *)

(* bitbucket.org/cannam/sml-utf8
   Copyright (c) 2015-2017 Chris Cannam (MIT license) *)

structure Utf8Decoder :> sig
    val foldlString : (word * word list -> word list) -> word list -> string -> word list
end = struct
    fun overlong n =
        case n of
          2 => 0wx0080
        | 3 => 0wx0800
        | 4 => 0wx10000
        | _ => 0wx0

    fun foldlString f a s =
        let open Word
            infix 6 orb andb xorb <<

            fun decode (byte, (n, i, cp, a)) =
                let val w = Word.fromLargeWord (Word8.toLargeWord byte)
                in case i of
                        0 => if (w andb 0wx80) = 0wx0  then (0, 0, 0wx0, f (w, a))
                        else if (w andb 0wxe0) = 0wxc0 then (2, 1, w xorb 0wxc0, a)
                        else if (w andb 0wxf0) = 0wxe0 then (3, 2, w xorb 0wxe0, a)
                        else if (w andb 0wxf8) = 0wxf0 then (4, 3, w xorb 0wxf0, a)
                        else (0, 0, 0wx0, f (0wxfffd, a))

                      | 1 => if w andb 0wxc0 = 0wx80 then
                        let val cp = (cp << 0w6) orb (w xorb 0wx80)
                        in if cp < overlong n  then (0, 0, 0wx0, f (0wxfffd, a))
                        else if cp > 0wx10ffff then (0, 0, 0wx0, f (0wxfffd, a))
                        else (0, 0, 0wx0, f (cp, a)) end
                        else decode (byte, (0, 0, 0wx0, f (0wxfffd, a)))

                      | i => if w andb 0wxc0 = 0wx80 then
                        let val cp = (cp << 0w6) orb (w xorb 0wx80)
                        in (n, Int.-(i, 1), cp, a)
                        end else decode (byte, (0, 0, 0wx0, f (0wxfffd, a)))
                end
        in case Word8Vector.foldl decode (0, 0, 0wx0, a) (Byte.stringToBytes s) of
                (n, 0, 0wx0, result) => result
              | (n, i, cp, result) => f (0wxfffd, result)
        end
end

structure Utf8Encoder :> sig
    val codepointsToUtf8 : ((word * char list -> char list) -> char list -> 'a -> char list) -> 'a -> string
    val codepointToUtf8 : word -> char list
end = struct
    open Word
    infix 6 orb andb >>
    val char = Char.chr o toInt
    fun prepend_utf8 (cp, acc) =
        if cp < 0wx80 then char cp :: acc
        else if cp < 0wx800 then
        char (0wxc0 orb (cp >> 0w6)) ::
        char (0wx80 orb (cp andb 0wx3f)) :: acc
        else if cp < 0wx10000 then
        char (0wxe0 orb (cp >> 0w12)) ::
        char (0wx80 orb ((cp >> 0w6) andb 0wx3f)) ::
        char (0wx80 orb (cp andb 0wx3f)) :: acc
        else if cp < 0wx10ffff then
        char (0wxf0 orb (cp >> 0w18)) ::
        char (0wx80 orb ((cp >> 0w12) andb 0wx3f)) ::
        char (0wx80 orb ((cp >> 0w6) andb 0wx3f)) ::
        char (0wx80 orb (cp andb 0wx3f)) :: acc
        else acc

    fun codepointToUtf8 cp =
        prepend_utf8 (cp, [])

    fun codepointsToUtf8 folder cps =
        String.implode (folder prepend_utf8 [] cps)

end

