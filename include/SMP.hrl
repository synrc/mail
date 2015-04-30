-ifndef(_SMP_HRL_).
-define(_SMP_HRL_, true).

-include_lib("kvs/include/kvs.hrl").

-define(GEN_SERVER, [start_link/1,init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).

-record('RosterItem',  {?ITERATOR(feed), name, surname, username, status}).
-record('MessageItem', {?ITERATOR(feed), recipient, payload, origin, status}).

-record('Auth',     {username, token, services}).
-record('Person',   {id, name, surname, username, status}).
-record('Presence', {size, userlist}).
-record('Friend',   {id, user, status}).
-record('Confirm',  {user, type}).
-record('Private',  {id, recipient, body, author, status}).
-record('Typing',   {room, author}).

-record('Room',     {room, description, acl, settings}).
-record('Join',     {user, room, answer}).
-record('Public',   {id, room, message, author, status}).

-record('Retrieve', {id, chat}).
-record('Mark',     {id, room, status}).
-record('Search',   {id, body, author}).

-endif.

