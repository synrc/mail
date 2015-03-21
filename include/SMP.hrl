-ifndef(_SMP_HRL_).
-define(_SMP_HRL_, true).

-record('Auth',{ username, token, services}).
-record('Person',{ id, name, surname, username, status}).
-record('Presence',{size, userlist}).
-record('Friend',{user, status}).
-record('Confirm',{user, type}).
-record('Room',{room, description, acl, settings}).
-record('Join',{user, room, answer}).
-record('Public',{id, room, message, author}).
-record('Private',{id, author, body, status}).
-record('Typing',{author}).
-record('Retrieve',{id, chat}).
-record('Mark',{id, room, status}).
-record('Search',{id, body, author}).

-endif. %% _SMP_HRL_
