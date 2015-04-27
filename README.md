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

Usage
-----

```erlang
> rr(roster).
> {ok,U} = roster:create("5HT","Maxim","Sokhatsky").
> roster:add(U,#'Person'{name=oleg,surname=zinchenko}).
> roster:add(U,#'Person'{name=rilian}).
> roster:list(1).

[#'Person'{id = 2,version = undefined,container = feed,
           feed_id = {1,roster},
           prev = 1,next = undefined,feeds = [],guard = false,
           etc = undefined,name = rilian,surname = undefined,
           username = undefined,status = undefined},
 #'Person'{id = 1,version = undefined,container = feed,
           feed_id = {1,roster},
           prev = undefined,next = 2,feeds = [],guard = false,
           etc = undefined,name = oleg,surname = zinchenko,
           username = undefined,status = undefined}]

> roster:private(1,2,#'Private'{body="Hi! :-)",author=1}).
> roster:private(1,2,#'Private'{body="Ok. Hi!",author=2}).
> roster:retrieve(1,2,undefined).

[#'Private'{id = 2,version = undefined,container = feed,
            feed_id = {1,chat,2},
            prev = 1,next = undefined,feeds = [],guard = false,etc = undefined,
            author = 2,body = "Ok. Hi!",status = undefined},
 #'Private'{id = 1,version = undefined,container = feed,
            feed_id = {1,chat,2},
            prev = undefined,next = 2,feeds = [],guard = false,
            etc = undefined,author = 1,body = "Hi! :-)",
            status = undefined}]
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
