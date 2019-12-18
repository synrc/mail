CHAT: Messaging Protocol
========================
[![Build Status](https://travis-ci.com/synrc/chat.svg?branch=master)](https://travis-ci.com/synrc/chat)
[![Hex pm](http://img.shields.io/hexpm/v/chat.svg?style=flat)](https://hex.pm/packages/chat)

Simple mailbox delivery protocol.

Features
--------

* Database Support: FS, MNESIA, ROCKSDB
* MQ Support: GPROC, SYN
* Formatters Support: TXT, BERT, BER/DER/PER [ASN.1]
* Size: 80 LOC

Intro
-----

CHAT is an QoS=1 example of messaging system built on top of:

* SYN for publish subscribe message queue;
* N2O for protocols;
* KVX for data storage;
* and COWBOY for web server.

It also contains simple textual WebSocket protocol for debugging purposes.
You can freely use this example with your favourite formatter for user terminal protocol.

```shell
$ wscat -c wss://n2o.im
> HELP
< N2O <user>
| SEND <user> <msg>
| BOX
| CUT <id>.
> N2O maxim
< USER maxim
> SEND vlad HELO
< ERROR user doesn't exist.
> N2O vlad
< USER vlad
> SEND maxim OK
< NOTIFY vlad:maxim:1556151953113322286:OK
< ACK "1556151953113322286"
> N2O maxim
< USER maxim
> BOX
< LIST
vlad:maxim:1556151953113322286:OK
> SEND maxim this is me
< NOTIFY maxim:maxim:1556152151055371152:this is me
< ACK "1556152151055371152"
> SEND maxim back again
< NOTIFY maxim:maxim:1556152157283311935:back again
< ACK "1556152157283311935"
> BOX
< LIST
vlad:maxim:1556151953113322286:OK
maxim:maxim:1556152151055371152:this is me
maxim:maxim:1556152157283311935:back again
> CUT 1556152157283311935
< ERASED 3
> BOX
< LIST
>
```

Documentation
-------------

The project homepage is <a href="https://chat.n2o.dev">chat.n2o.dev</a>.

Credits
-------

* Maxim Sokhatsky
* Vladimir Kirillov

OM A HUM
