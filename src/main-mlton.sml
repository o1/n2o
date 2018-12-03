structure Main = struct
open MLton.Signal
fun main (program_name, arglist) =
    (setHandler (Posix.Signal.pipe, Handler.ignore);
     RunCML.doit (fn () => Server.run(program_name, arglist), NONE);
     OS.Process.success)
end

val _ = Main.main ("test", nil)
