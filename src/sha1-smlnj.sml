structure SHA1 :> sig
              val sha : Word8Vector.vector -> Word8Vector.vector
          end = struct
fun sha _ = raise Fail "unimplemented: SHA1"
end
