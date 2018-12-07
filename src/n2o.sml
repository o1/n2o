functor N2O(M : sig type 'a Prot end) = struct

type 'a Prot = 'a M.Prot (* Input type for protocol handler *)
datatype 'a Ev = Init | Message of 'a | Terminate (* Output type for protocol handler and input type for event handler *)
datatype 'a Res = Reply of 'a | Ok | Error (* Output type for event handler *)

type Req = { path : string,
             method : string,
             version : string,
             headers : (string * string) list }

datatype 'a Cx = Cx of { req : Req, proto : 'a Prot -> 'a Ev,  module : 'a Ev -> 'a Res, handlers : ('a Hnd) list }
withtype 'a Hnd = 'a Cx -> 'a Cx

fun run (cx :'a Cx) (msg : 'a Prot) : 'a Res =
    case cx of
        Cx {module,handlers,proto,...} =>
        (List.foldr (fn (h,c) => h c) cx handlers;
         module (proto msg))
end

structure Ex = struct

datatype 'a Nitro = Init of string
                  | Pickle of string*string*((string*string) list)
                  | Done

datatype 'a N2OProt = Nitro of 'a
                    | IO of string*string

end
