Roster N2O Protocol
===================

Roster protocol is a part of N2O IoT and WebSocket
protocol stack for messaging applications and server implementations.
Roster protocol has several sub-protocols, containing following messages:

```erlang
% roster
-record('RosterItem',  {?ITERATOR(feed), name, surname, username, status}).
-record('MessageItem', {?ITERATOR(feed), recipient, payload, origin, status}).

-record('Auth',     {username, token, services}).
-record('Person',   {id, name, surname, username, status}).
-record('Presence', {size, userlist}).
-record('Friend',   {id, user, status}).
-record('Confirm',  {user, type}).
-record('Private',  {id, recipient, body, author, status}).
-record('Typing',   {room, author}).

% muc
-record('Room',     {room, description, acl, settings}).
-record('Join',     {user, room, answer}).
-record('Public',   {id, room, message, author, status}).

% search
-record('Retrieve', {id, chat}).
-record('Mark',     {id, room, status}).
-record('Search',   {id, body, author}).
```

Usage
-----

```erlang
> rr(roster).
> {ok, {Pid, Uid}} = roster:create_user("5HT","Maxim","Sokhatsky"). % create_user(User,Name,Surname)
> {ok,C1} = roster:add(Uid,#'RosterItem'{name=oleg,surname=zinchenko}).
> {ok,C2} = roster:add(Uid,#'RosterItem'{name=rilian}).
> roster:list(Uid).

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

> roster:private(Uid,C1,#'MessageItem'{payload="Hi! :-)"}).
> roster:private(Uid,C1,#'MessageItem'{payload="How's it going?"}).
> roster:private(Uid,C2,#'MessageItem'{payload="Ok. Hi!"}).
> roster:retrieve({chat,C1},9).

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
