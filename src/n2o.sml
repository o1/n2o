signature PROTO = sig
    type Prot (* Input type for protocol handler *)
    type Ev (* Output type for protocol handler and input type for event handler *)
    type Res
    type Req
    val proto : Prot -> Ev
end

functor MkN2O(M : PROTO) = struct
type Cx = {req: M.Req, module: M.Ev -> M.Res}
fun run (cx : Cx) (handlers : (Cx -> Cx) list) (msg : M.Prot) =
    (#module cx) (M.proto msg)
end
