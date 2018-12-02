structure Server = MkServer(
    struct
    open TextIO
    fun setPipeHandler () =
       ignore ( UnixSignals.setHandler (UnixSignals.sigPIPE, UnixSignals.IGNORE) )
    end)

