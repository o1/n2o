N2O for Standard ML
===================

Here is example echo server, using N2O HTTP static and WebSocket servers.

Build
-----
[![Build Status](https://travis-ci.org/o1/n2o.svg?branch=master)](https://travis-ci.org/o1/n2o)

Unix, Linux, Mac:

```bash
$ mlton n2o.mlb && ./n2o
$ ml-build n2o.cm Main.main n2o.nj && sml @SMLload=n2o.nj
```

Windows:

```bash
$ ml-build n2o-win.cm Main.main n2o-win.nj && sml @SMLload=n2o-win.nj
```

Run
---

Open http://localhost:8989 or use `wscat`

```
$ wscat -c ws://127.0.0.1:8989/ws/
connected (press CTRL+C to quit)
> helo
< helo
```

Measure
-------

```
$ tcpkali -T20s -r 100000 -c 2 -m PING —latency-marker "PING" —ws 127.0.0.1:8989/ws
Destination: [127.0.0.1]:8989
Interface lo address [127.0.0.1]:0
Using interface lo to connect to [127.0.0.1]:8989
Ramped up to 2 connections.
Total data sent:     33.0 MiB (34637100 bytes)
Total data received: 18.4 MiB (19263720 bytes)
Bandwidth per channel: 10.779⇅ Mbps (1347.4 kBps)
Aggregate bandwidth: 7.705↓, 13.853↑ Mbps
Packet rate estimate: 23465.4↓, 1327.3↑ (1↓, 1↑ TCP MSS/op)
Message latency at percentiles: 5043.1/5327.9/5375.9 ms (95/99/99.5%)
Test duration: 20.0022 s.
```

Credits
-------

* Marat Khafizov
* Maxim Sokhatsky
