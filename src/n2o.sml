signature PROTO = sig
    type Prot (* Input type for protocol handler *)
    type Ev (* Output type for protocol handler and input type for event handler *)
    type Res
    type Req
    val proto : Prot -> Ev
end

signature N2O = sig
    type Cx
    type Prot
    type Res
    val run : Cx -> Prot -> Res
end

functor N2O(M : PROTO) : N2O = struct
    datatype Cx = Cx of { req : M.Req, module :  M.Ev -> M.Res, handlers : Hnd list }
    withtype Hnd = Cx -> Cx
    fun run (cx : Cx) (msg : M.Prot) : M.Res =
        case cx of
            Cx {module,handlers,...} =>
            (List.foldr (fn (h,c) => h c) cx handlers;
             module (M.proto msg))
end

structure Example : PROTO = struct
  datatype Ev = Start of string | Message of string | Done
  type Res = WebSocket.Res
  type Req = Server.Req
  datatype Nitro = Init of string
                 | Pickle of string*string*((string*string) list)
                 | Terminate
                 | IO of string*string
  type Prot = Nitro
  fun proto msg = case msg of
                      (Init s) => Start s
                    | (Pickle _) => (print "got pickled msg\n"; Message "hi")
                    | Terminate => Done
                    | _ => Message "unknown message"
end

structure ExN2O = N2O(Example)
