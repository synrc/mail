CHAT: Messaging Protocol
========================
[![Build Status](https://travis-ci.org/synrc/chat.svg?branch=master)](https://travis-ci.org/synrc/chat)

CHAT is an QoS=1 example of messaging system built on top of:

* SYN for publish subscribe message queue;
* N2O for protocols;
* KVX for data storage;
* and COWBOY for web server.

It also contains simple textual WebSocket protocol for debugging purposes.
You can freely use this example with your favourite formatter for user terminal protocol.

```shell
$ wscat --no-check -c https://localhost:8042
connected (press CTRL+C to quit)
> N2O,3
< USER 3
> N2O,5
< USER 5
> MSG 3 5 0 HELO
< SENT 1555353999252659000
< ACK 1555353999252659000
> MSG 3 5 0 KITTY
< SENT 1555354008545233000
< ACK 1555354008545233000
> MSG 3 5 QoS=1 BYE 
< SENT "QoS=1"
< ACK "QoS=1"
> MSG 3 5 QoS=1 BYE 
< SENT "QoS=1"
< ACK "QoS=1"
> HIST 3 5
< History:
3:5:HELO
3:5:KITTY
3:5:BYE
```

```erlang
> kvx:all({p2p,"3","5"}).
[{'Message',"QoS=1",[],"3","5",
            [{'File',[],<<"text">>,"BYE",[],[]}],
            [],[],[]},
 {'Message',1555353999252659000,[],"3","5",
            [{'File',[],<<"text">>,"HELO",[],[]}],
            [],[],[]},
 {'Message',1555354008545233000,[],"3","5",
            [{'File',[],<<"text">>,"KITTY",[],[]}],
            [],[],[]}]
```

Documentation
-------------

The project homepage is <a href="https://n2o.im">n2o.im</a>.
The documentation on stack is in header and by the following address <a href="https://o7.n2o.space">o7.n2o.space</a>.

Credits
-------

* Maxim Sokhatsky
* Vladimir Kirillov

OM A HUM
