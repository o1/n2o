N2O for Standard ML
===================

Here is yet simple HTTP static server implemented using
CML loop with INetSock.TCP acceptor loop.

```
$ mlton n2o.mlb && ./n2o
$ ml-build n2o.cm Main.main n2o.nj && sml @SMLload=n2o.nj
```

Open http://localhost:8989/index

Credits
-------

* Marat Khafizov
* Maxim Sokhatsky
