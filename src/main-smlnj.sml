structure Server = MkServer(
    struct
    fun setPipeHandler () =
       ignore ( UnixSignals.setHandler (UnixSignals.sigPIPE, UnixSignals.IGNORE) )
    end)

