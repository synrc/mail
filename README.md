Usage of Synrc Messaging Protocol
=================================

This protocol contains following messages:

```erlang
-record('Auth',{ username, token, services}).
-record('Person',{ id, name, surname, username, status}).
-record('Presence',{ size, userlist}).
-record('Add',{ user, status}).
-record('Confirm',{ user, type}).
-record('Room',{ name, id}).
-record('Create',{ room, description, acl, settings}).
-record('Join',{ user, room, answer}).
-record('RoomEvent',{ id, room, message, author}).
-record('RoomMessage',{ id, author, room, body, status}).
-record('Message',{ id, author, body, status}).
-record('Typing',{ author}).
-record('Retrieve',{ id, chat}).
-record('Mark',{ id, room, status}).
-record('Search',{ id, body, author}).
```

```erlang

>  iolist_to_binary(smp_js:auth("maxim","123","12")).
<<"ws.send(enc(tuple(atom('Auth'),bin('maxim'),bin('123'),'12')));">>

```

Credits
-------

* Maxim Sokhatsky
* Oleg Zinchenko

OM A HUM
