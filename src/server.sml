structure EchoHandler : HANDLER = struct
    fun hnd (_,msg) = msg
end

structure Server = MkServer(EchoHandler)
