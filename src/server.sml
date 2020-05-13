structure EchoProto : PROTO = struct
    type Prot = WebSocket.Msg
    type Ev = Word8Vector.vector option
    type Res = WebSocket.Res
    type Req = HTTP.Req
    fun proto (WebSocket.TextMsg s) = SOME s
      | proto _ = NONE
end
structure Echo = MkN2O(EchoProto)

structure EchoHandler : HANDLER = struct

    fun noop _ = WebSocket.Ok
    fun echo NONE = WebSocket.Ok
      | echo (SOME s) = WebSocket.Reply (WebSocket.TextMsg s)
    fun router (cx : Echo.Cx) =
        {req=(#req cx),module=echo}
    fun hnd (req,msg) =
        Echo.run {req=req,module=noop} [router] msg
end

structure Server = MkServer(EchoHandler)
