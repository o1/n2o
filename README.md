N2O for Standard ML
===================

Here is yet simple HTTP static server implemented using
CML loop with INetSock.TCP acceptor loop.

```
$ mlton n2o.mlb && ./n2o
$ ml-build n2o.cm Server.main n2o.nj && exec sml @SMLload=n2o.nj
```

Go to http://localhost:8989/index

```bash
$ curl -XGET -v "http://localhost:8989/foobar"
Note: Unnecessary use of -X or --request, GET is already inferred.
*   Trying 127.0.0.1...
* TCP_NODELAY set
* Connected to localhost (127.0.0.1) port 8989 (#0)
> GET /foobar HTTP/1.1
> Host: localhost:8989
> User-Agent: curl/7.58.0
> Accept: */*
> 
< HTTP/1.1 404 Not Found
* Connection #0 to host localhost left intact

```

Credits
-------

* Marat Khafizov
* Maxim Sokhatsky

