CHAT: Messaging Protocol
========================
[![Build Status](https://travis-ci.org/synrc/chat.svg?branch=master)](https://travis-ci.org/synrc/chat)

CHAT is an QoS=1 example of messaging system built on top of:
1) SYN/GRPOC for publish subscribe;
2) N2O for protocol description;
3) KVX for data storage; and 4) COWBOY for Web Server.
It also contains simple TEXT WebSocket protocol for debuggin purposes.

```shell
$ wscat --no-check -c https://localhost:8042
connected (press CTRL+C to quit)
> N2O,3
< USER 3
> N2O,5
< USER 5
> MSG 3 5 HELO
< SENT 1555353999252659000
< ACK 1555353999252659000
> MSG 3 5 KITTY
< SENT 1555354008545233000
< ACK 1555354008545233000
> MSG 3 5 BYE QoS=1
< SENT "QoS=1"
< ACK "QoS=1"
> MSG 3 5 BYE QoS=1
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

The site of the project is <a href="https://n2o.im">n2o.im</a>.

Credits
-------

* Maxim Sokhatsky
* Vladimir Kirillov

OM A HUM
