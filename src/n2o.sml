signature PROTO = sig
    type Prot (* Input type for protocol handler *)
    type Ev (* Output type for protocol handler and input type for event handler *)
    type Res type Req
    val proto : Prot -> Ev
end

functor MkN2O(M : PROTO) = struct
    type Cx = {req: M.Req, module: M.Ev -> M.Res}
    fun fold cx [] = cx
      | fold cx (head::tail) =
        let val cx1 = head cx in
        fold cx1 tail end
    fun run (cx : Cx) (handlers : (Cx -> Cx) list) (msg : M.Prot) =
        let val cx1 = fold cx handlers in
        (#module cx1) (M.proto msg) end
end
