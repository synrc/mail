-module(roster_index).
-compile(export_all).
-include_lib("kvs/include/feed.hrl").
-include_lib("kvs/include/user.hrl").
-include_lib("kvs/include/group.hrl").
-include_lib("n2o/include/wf.hrl").

main()    -> #dtl{file="index",app=roster,bindings=[{body,body()}]}.
body()    -> lists:flatten(
                [ #label{body=lists:concat(["Logged as: ",wf:user()])},
                  #h1{body="Users"},
                  [ [ #link{href=lists:concat(["chat?user=",U#user.id]),
                            body=string:join([U#user.names,U#user.surnames]," ")},
                      #br{} ]  || U <- list(user) ],
                  #h1{body="Groups"},
                  [ [ #link{href=lists:concat(["chat?room=",G#group.id]),
                            body=lists:concat([G#group.id])},
                      #br{} ] || G <- list(group)] ]).

list(Space) -> case kvs:get(feed,Space) of
                      {ok,Feed} ->
                            kvs:fold(fun(A,Acc) -> [A|Acc] end,[],Space,
                            Feed#feed.top,10,
                            #iterator.prev,#kvs{mod=store_mnesia});
                  __ -> [] end.

event(Event) -> wf:info(?MODULE,"Unknown Event: ~p~n",[Event]).
