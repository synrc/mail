-module(roster_api).
-author('Maxim Sokhatsky').
-compile(export_all).
-include_lib("kvs/include/entry.hrl").
-include_lib("n2o/include/wf.hrl").

main() -> #dtl{app=roster}.

event(init) -> roster_auth:auth(),
               wf:info(?MODULE,"AUTH: ~p~n",[wf:session(<<"session">>)]);

event(Event) -> wf:info(?MODULE,"Unknown Event: ~p~n",[Event]).
