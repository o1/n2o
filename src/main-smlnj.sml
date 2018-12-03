structure Main = struct
open TextIO
fun main (program_name, arglist) =
    (UnixSignals.setHandler (UnixSignals.sigPIPE, UnixSignals.IGNORE);
     RunCML.doit (fn () => Server.run(program_name, arglist), NONE);
     OS.Process.success)
end
