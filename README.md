ROSTER: Messaging Protocol
==========================

Roster protocol is a part of N2O IoT and WebSocket
protocol stack for messaging applications and server implementations.
Roster protocol has several sub-protocols, containing following messages:

```erlang
-record('Message',      {id        = [] :: [] | integer(),
                         next      = [] :: [] | integer(),
                         prev      = [] :: [] | integer(),
                         container = chain :: container(),
                         feed_id   = [] :: #muc{} | #p2p{},
                         msg_id    = [] :: [] | binary(),
                         from      = [] :: [] | binary(),
                         to        = [] :: [] | binary(),
                         created   = [] :: [] | integer(),
                         files     = [] :: list(#'File'{}),
                         type      = [] :: messageType(),
                         link      = [] :: [] | integer() | #'Message'{},
                         msg_status= [] :: messageStatus()}).

-record('History',      {roster_id = [] :: [] | binary(),
                         feed      = [] :: [] | #p2p{} | #muc{},
                         size      = 0  :: [] | integer(),
                         entity_id = 0  :: [] | integer(),
                         data      = [] :: integer(),
                         status    = [] :: historyType()}).

```

Credits
-------

* Maxim Sokhatsky

OM A HUM
