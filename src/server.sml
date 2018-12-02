functor MkServer(M: SIGNAL) = struct
open TextIO
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

fun serve sock : Resp =
    let val slc = Word8VectorSlice.full (Socket.recvVec (sock, 2048))
    in case tokens slc "\r\n" of
            nil => raise BadRequest
         |  (hd::tl) =>
            case map (Byte.bytesToString o Word8VectorSlice.vector) (tokens hd " ") of
                 nil => raise BadRequest
              | "GET"::path::_ =>
                let val reqPath = "static/html/" ^ path ^ ".html"
                 in let val stream = BinIO.openIn reqPath
                        val data = BinIO.inputAll stream
                        val () = BinIO.closeIn stream
                     in { status = 200,
                          headers = [("Content-Type", "text/html"),
                                     ("Content-Length", Int.toString (Word8Vector.length data))],
                          body = data }
                    end
                    handle Io => raise NotFound reqPath
                end
              | _ => raise BadRequest
    end

fun sendBytes sock bytes =
    ignore (Socket.sendVec (sock, Word8VectorSlice.full bytes))
    before Socket.close sock

fun sendList sock lst =
    sendBytes sock (Word8Vector.concat lst)

fun sendStr sock str = sendBytes sock (Byte.stringToBytes str)

fun connMain sock =
    let
        val resp = serve sock
        val headers = #headers resp
        val body = #body resp
    in
    sendList
        sock
        ([Byte.stringToBytes "HTTP/1.1 200 OK\r\n"]
         @ (List.map
                (fn (k,v) => Byte.stringToBytes (k ^ ": " ^ v ^ "\r\n"))
                headers)
            @ [Byte.stringToBytes "\r\n", body]
           )
         end
         handle BadRequest    => print "Bad Request\n"
                                 before sendStr sock "HTTP/1.1 400 Bad Request\r\n"
              | NotFound path => (print "Not Found\n")
                                 before sendStr sock "HTTP/1.1 404 Not Found\r\n"

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

