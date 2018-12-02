datatype 'a Ev =
         Init
         | Message of 'a
         | Terminate
datatype 'a Res =
         Reply of 'a
         | Ok
         | Unknown
         | Empty
type Req = { path : string, method : string,
             version : string, headers : (string * string) list
           }
fun mkReq () =
    { path = "", method = "", version = "HTTP/1.1", headers = [] }
type 'a Proto = 'a -> 'a Res
type 'a Cx = { req : Req, module : 'a Ev -> 'a Res
             }
type 'a Hnd = 'a Cx -> 'a Cx
val 'a mod_ : 'a Ev -> 'a Res = fn _ => Empty

signature BASE =
sig
    type t
    type 'a prot
end

signature BASE_EXT =
sig
    include BASE
    val handlers : (t Hnd) list
    val protos : ((t prot) Proto) list
end

signature CX = sig
    include BASE_EXT
    val cx : t Cx
end

functor MkCx(M: BASE_EXT) :> CX = struct
type t = M.t
type 'a prot = 'a M.prot
val cx = { req = mkReq ():Req, module = mod_ } : t Cx
val handlers = M.handlers
val protos = M.protos
end

signature N2O = sig
    include BASE
    val run : t prot -> ((t prot) Proto) list -> (t prot) Res
end

functor MkN2O(M: BASE_EXT) = struct
type t = M.t
type 'a prot = 'a M.prot
structure Ctx = MkCx(M)
fun run msg =
    let
        fun loop _ [] = Empty
          | loop msg (proto::protos) =
            case proto msg of
                Unknown    => loop msg protos
              | Empty      => Empty
              | Reply msg1 => Reply msg1
              | _          => loop msg protos
        fun fold cx [] = cx
          | fold cx (h::hs) = fold (h cx) hs
    in
        fold Ctx.cx Ctx.handlers;
        loop msg Ctx.protos
    end
end

(* protocols *)
datatype 'a Nitro = Init of string
                  | Pickle of string*string*((string*string) list)
                  | Done
datatype 'a N2OProt = Nitro of 'a
                    | IO of string*string

(* example *)
datatype Example = Greet
structure Test = MkN2O(struct
                        type t = Example
                        type 'a prot = 'a N2OProt
                        val handlers = []
                        val protos = []
                        end)
