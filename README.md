Roster N2O Protocol
===================

Roster protocol is a part of N2O IoT and WebSocket
protocol stack for messaging applications and server implementations.
Roster protocol has several sub-protocols, containing following messages:

```erlang
% roster
-record('Auth',{username, token, services}).
-record('Person',{id, name, surname, username, status}).
-record('Presence',{size, userlist}).
-record('Friend',{user, status}).
-record('Confirm',{user, type}).
-record('Private',{id, author, body, status}).
-record('Typing',{author}).

% muc
-record('Room',{room, description, acl, settings}).
-record('Join',{user, room, answer}).
-record('Public',{id, room, message, author}).

% search
-record('Retrieve',{id, chat}).
-record('Mark',{id, room, status}).
-record('Search',{id, body, author}).
```

You may refer to full N2O protocol here: http://5ht.github.io/n2o.htm

```erlang

>  iolist_to_binary(smp_js:auth("maxim","123","12")).
<<"ws.send(enc(tuple(atom('Auth'),bin('maxim'),bin('123'),'12')));">>

```

Credits
-------

* Maxim Sokhatsky
* Oleg Zinchenko

OM A HUM
