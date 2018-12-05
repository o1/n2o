structure Compat = struct

fun w8_to_w64 w8 =
    if LargeWord.wordSize = 64
    then Word64.fromLarge (Word8.toLarge w8)
    else Word64.fromLargeInt (Word8.toLargeInt w8)

fun w64_to_w8 w64 =
    if LargeWord.wordSize = 64
    then Word8.fromLarge (Word64.toLarge w64)
    else Word8.fromLargeInt (Word64.toLargeInt w64)

end
