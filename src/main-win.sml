structure Main = struct
open TextIO
fun main (program_name, arglist) =
    (RunCML.doit (fn () => Server.run(program_name, arglist), NONE);
     OS.Process.success)
end
