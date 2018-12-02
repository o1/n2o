functor MkServer(M: SIGNAL) = struct
type Req = { path : string, headers : (string*string) list }
type Resp = { status : int, headers : (string*string) list, body : Word8Vector.vector }
exception BadRequest
exception NotFound of string

fun collect mark i sepLen acc slc =
    if i > (mark + sepLen) then
         (Word8VectorSlice.subslice (slc, mark, SOME ((i-mark)-sepLen)))::acc
    else acc

fun recur s l len sepLen mark i [] acc =
    recur s l len sepLen i i l (collect mark i sepLen acc s)
  | recur s l len sepLen mark i (b::bs) acc =
    if i = len then List.rev (collect mark i 0 acc s)
    else recur s l len sepLen mark (i+1)
         (if b = Word8VectorSlice.sub (s, i) then bs else l) acc

fun tokens slc (sep : string) =
    let val lst = map (Word8.fromInt o Char.ord) (String.explode sep)
        val len = Word8VectorSlice.length slc
        val sepLen = String.size sep
    in recur slc lst len sepLen 0 0 lst [] end

val sliceToStr = Byte.bytesToString o Word8VectorSlice.vector
fun tokens' slc (sep : string) = map sliceToStr (tokens slc sep)

fun parseHeaders nil = nil
  | parseHeaders (ln::lns) = (case tokens' ln ": " of
                                  k::v::_ => (k,v)
                                | _ => raise BadRequest) :: (parseHeaders lns)

fun parseReq slc : Req =
    case tokens slc "\r\n" of
        nil => raise BadRequest
      | lines as (hd::tl) =>
        case tokens' hd " " of
            "GET"::path::_ => { path = path, headers = parseHeaders tl }
          | _ => raise BadRequest

fun needUpgrade req = false (*TODO*)

fun serve sock : Resp =
    let
        val req = parseReq (Word8VectorSlice.full (Socket.recvVec (sock, 2048)))
        val reqPath = case #path req of
                          "/" => "/index"
                        | p => if String.isPrefix "/ws" p
                               then String.extract (p, 3, NONE)
                               else p
    in
        if needUpgrade req then
            raise BadRequest (*TODO*)
        else
            let val filePath = "static/html" ^ reqPath ^ ".html"
            in let val stream = BinIO.openIn filePath
                   val data = BinIO.inputAll stream
                   val () = BinIO.closeIn stream
               in { status = 200,
                    headers = [("Content-Type", "text/html"),
                               ("Content-Length", Int.toString (Word8Vector.length data))],
                    body = data }
               end
               handle Io => raise NotFound filePath
            end
    end

fun sendBytes sock bytes = ignore (Socket.sendVec (sock, Word8VectorSlice.full bytes))
fun sendList sock lst = sendBytes sock (Word8Vector.concat lst)
fun sendStr sock str = sendBytes sock (Byte.stringToBytes str)

fun respCode 200 = "OK"
  | respCode 400 = "Bad Request"
  | respCode 404 = "Not Found"
  | respCode _ = "Internal Server Error"

fun sendResp sock {status=status,headers=headers,body=body} =
    sendList
        sock
        ([Byte.stringToBytes ("HTTP/1.1 "^Int.toString status^" "^respCode status^"\r\n")]
         @ (List.map
                (fn (k,v) => Byte.stringToBytes (k ^ ": " ^ v ^ "\r\n"))
                headers)
         @ [Byte.stringToBytes "\r\n", body])

fun sendError sock code body =
    print body
    before sendResp sock {status=code,headers=[],body=Byte.stringToBytes body}
    before Socket.close sock

fun connMain sock =
    (case serve sock of
         resp => sendResp sock resp before Socket.close sock)
    handle BadRequest    => sendError sock 400 "Bad Request\n"
         | NotFound path => sendError sock 404 ("Not Found: "^path^"\n")

fun acceptLoop server_sock =
    let val (s, _) = Socket.accept server_sock
    in
        print "Accepted a connection.\n";
        CML.spawn (fn () => connMain(s));
        acceptLoop server_sock
    end

fun cml_main (program_name, arglist) =
    let val s = INetSock.TCP.socket()
    in
    Socket.Ctl.setREUSEADDR (s, true);
    Socket.bind(s, INetSock.any 8989);
    Socket.listen(s, 5);
    print "Entering accept loop...\n";
    acceptLoop s
    end

fun main (program_name, arglist) =
    (M.setPipeHandler ();
     RunCML.doit (fn () => cml_main(program_name, arglist), NONE);
     OS.Process.success)
end

