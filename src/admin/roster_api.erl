-module(roster_api).
-author('Maxim Sokhatsky').
-compile(export_all).
-include_lib("kvs/include/entry.hrl").
-include_lib("n2o/include/wf.hrl").
-include("roster_api.hrl").

main() ->
    #dtl{app=roster}.

event(init) ->
    roster_auth:auth(),
    wf:info(?MODULE,"AUTH: ~p ~p~n",[wf:session(<<"session">>),wf:user()]);

event({bin,#'GetRoomList'{}}) ->
    Result = kvs:entries(kvs:get(feed,group),group,undefined),
    wf:info(?MODULE,"GetRoomList: ~p~n",[Result]),
    Result;

event({bin,#'GetUserInfo'{}}) ->
    Result = kvs:get(user,wf:user()),
    Result;

event(Event) ->
    wf:info(?MODULE,"Unknown Event: ~p~n",[Event]).
