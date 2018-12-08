signature PROTO = sig
    type Prot (* Input type for protocol handler *)
    type Ev (* Output type for protocol handler and input type for event handler *)
    type Res
    type Req
    val proto : Prot -> Ev
end

signature N2O = sig
    type Cx
    structure P : PROTO
    val run : Cx -> (Cx -> Cx) list -> P.Prot -> P.Res
end
