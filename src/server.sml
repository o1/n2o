structure Http = struct

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

fun reply data =
    { status   = 200,
      body     = data,
      headers  = [("Content-Type", "text/html"),
                  ("Content-Length", Int.toString (Word8Vector.length data))] }

fun processReq req =
    let val stream   = BinIO.openIn req
        val data     = BinIO.inputAll stream
        val ()       = BinIO.closeIn stream
     in reply data end handle Io => raise NotFound req

fun processPath path =
    processReq ("static/html/" ^ path ^ ".html")

fun unreq hd = case map (Byte.bytesToString o Word8VectorSlice.vector) (tokens hd " ") of
    nil => raise BadRequest
    | "GET"::path::_ => processPath path
    | _ => raise BadRequest

fun untokens list = case list of
    nil => raise BadRequest
    | hd::tl => unreq hd

fun recv sock : Resp =
    untokens (tokens (Word8VectorSlice.full (Socket.recvVec (sock, 2048))) "\r\n")

fun sendStr sock str =
    ignore (Socket.sendVec (sock, Word8VectorSlice.full (Byte.stringToBytes str)))
    before Socket.close sock end

structure Server = struct

open TextIO
open Http

fun connMain sock =
    let
        val r = recv sock
    in
        sendStr sock ("HTTP/1.1 200 OK\r\n"
                      ^ (String.concat
                             (List.map
                                  (fn (k,v) => k ^ ": " ^ v ^ "\r\n")
                                  (#headers r)))
                      ^ "\r\n"
                      ^ (Byte.bytesToString (#body r))) (* TODO: send bytes *)
    end
    handle BadRequest    => print "Bad Request"
                            before sendStr sock "HTTP/1.1 400 Bad Request\r\n"
         | NotFound path => print "Not Found"
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
    let
        open MLton.Signal
    in
        (setHandler (Posix.Signal.pipe, Handler.ignore);
         RunCML.doit (fn () => cml_main(program_name, arglist), NONE);
         OS.Process.success)
    end
end

val _ = Server.main ("test", nil)
