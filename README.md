N2O for Standard ML
===================

Here is example echo server, using N2O HTTP static and WebSocket servers.

```
$ mlton n2o.mlb && ./n2o
$ ml-build n2o.cm Main.main n2o.nj && sml @SMLload=n2o.nj
```

Open http://localhost:8989

Credits
-------

* Marat Khafizov
* Maxim Sokhatsky
