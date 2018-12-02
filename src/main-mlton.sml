structure Server = MkServer(
    struct
    open MLton.Signal;
    fun setPipeHandler () = ignore (Posix.Signal.pipe, Handler.ignore)
    end)

val _ = Server.main ("test", nil)
