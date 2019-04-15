CHAT: Messaging Protocol
========================

Roster protocol is a part of N2O IoT and WebSocket
protocol stack for messaging applications and server implementations.
Roster protocol has several sub-protocols, containing following messages:

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

Credits
-------

* Maxim Sokhatsky

OM A HUM
