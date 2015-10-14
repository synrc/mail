-module(roster_index).
-compile(export_all).
-include_lib("kvs/include/user.hrl").
-include_lib("kvs/include/group.hrl").
-include_lib("nitro/include/nitro.hrl").

main()    -> #dtl{app=roster,bindings=[{body,body()}]}.
body()    -> lists:flatten(
                [ #label{body=lists:concat(["Logged as: ",(wf:user())#user.id])},
                  #h1{body="Users"},
                  [ [ #link{href=lists:concat(["chat?user=",U#user.id]),
                            body=string:join([U#user.names,U#user.surnames]," ")},
                      #br{} ]  || U <- list(user) ],
                  #h1{body="Groups"},
                  [ [ #link{href=lists:concat(["chat?room=",G#group.id]),
                            body=lists:concat([G#group.id])},
                      #br{} ] || G <- list(group)] ]).

list(Space) -> kvs:entries(kvs:get(feed,Space),Space,10).

event(E) -> roster_api:event(E).
