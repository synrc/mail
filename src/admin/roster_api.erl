-module(roster_api).
-author('Maxim Sokhatsky').
-compile(export_all).
-include_lib("nitro/include/nitro.hrl").
-include("websocket_api.hrl").

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
    wf:user();

event(Event) ->
    wf:info(?MODULE,"Unknown Event: ~p~n",[Event]).
