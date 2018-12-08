structure EchoProto : PROTO = struct
type Prot = WebSocket.Msg
type Ev = string option
type Res = WebSocket.Res
type Req = HTTP.Req
fun proto (WebSocket.TextMsg s) = SOME(Byte.bytesToString s)
  | proto _ = NONE
end

structure EchoHandler : HANDLER = struct
structure Echo = MkN2O(EchoProto)
fun echo NONE = WebSocket.Ok
  | echo (SOME s) = raise Fail ""
fun router ({req,...} : Echo.Cx) =
    {req=req,module=echo}
fun hnd (req,msg) =
    Echo.run {req=req,module=echo} [router] msg
end

structure Server = MkServer(EchoHandler)
