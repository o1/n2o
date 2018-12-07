signature PROTO = sig
    type a (* type param *)
    type Prot (* Input type for protocol handler *)
    type Ev (* Output type for protocol handler and input type for event handler *)
    type Res (* Output type for event handler *)
    val proto : Prot -> Ev
end

functor N2O(M : PROTO) = struct

  type Req = { path : string,
               method : string,
               version : string,
               headers : (string * string) list }

  datatype Cx = Cx of { req : Req, module :  M.Ev -> M.Res, handlers : Hnd list }
  withtype Hnd = Cx -> Cx

  fun run (cx : Cx) (msg : M.Prot) : M.Res =
      case cx of
          Cx {module,handlers,...} =>
          (List.foldr (fn (h,c) => h c) cx handlers;
           module (M.proto msg))
end

structure Example : PROTO = struct

  type a = string

  datatype Ev = Start of string | Message of a | Done
  datatype Res = Reply of a | Error of string | Ok

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
