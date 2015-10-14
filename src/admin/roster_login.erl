-module(roster_login).
-compile(export_all).
-include_lib("kvs/include/feed.hrl").
-include_lib("kvs/include/user.hrl").
-include_lib("kvs/include/group.hrl").
-include_lib("nitro/include/nitro.hrl").

main() -> #dtl{app=roster,bindings=[{body,body()}]}.

body() ->
 [ #span   { id=display },                #br{},
   #span   { body="Login: " },            #textbox{id=user,autofocus=true},
   #button { body="Login",postback=login,source=[user,pass]}].

event(init) -> ok;

event(login) ->
    User = case wf:q(user) of <<>> -> "anonymous";
                              undefined -> "anonymous";
                              E -> wf:to_list(E) end,
    wf:user(#user{id=User}),
    wf:info(?MODULE,"User: ~p",[wf:user()]),
    wf:redirect("/index");

event(E) -> roster_api:event(E).
