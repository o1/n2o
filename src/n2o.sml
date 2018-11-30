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
             version : string, headers : string * string list
           }
type 'a Proto = 'a -> 'a Res
type 'a Cx = { req : Req, module : 'a Ev -> 'a Res
             }
type 'a Hnd = 'a Cx -> 'a Cx

signature CX = sig
    type t
    type 'a prot
    val cx : t Cx
    val handlers : (t Hnd) list
    val protos : ((t prot) Proto) list
end
