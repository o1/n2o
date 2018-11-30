structure N2O = struct

open TextIO

datatype 'a cx1 = MkCx of string * ('a hnd)
and 'a hnd = MkHnd of 'a cx1 -> 'a cx1;

structure SU = SockUtil

fun connMain s =
    let fun count 0 = SU.sendStr (s, "Bye!\r\n")
      | count n = (SU.sendStr (s, "Hello " ^ (Int.toString n) ^ "\r\n");
               CML.sync (CML.timeOutEvt (Time.fromReal 0.5));
               count (n - 1))
    in
    count 10;
    print "Closing the connection.\n";
    Socket.close s
    end

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

val _ = N2O.main ("test", nil)
