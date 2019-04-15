-module(roster_proto).
-include("message.hrl").
-include_lib("kvx/include/cursors.hrl").
-include_lib("n2o/include/n2o.hrl").
-compile(export_all).

info(#'Message'{from=From,to=To,id=Id}=M, R, S) ->
   case kvx:get({p2p,From,To},Id) of
        {ok,_} -> skip;
        {error,_} -> kvx:add((kvx:writer({p2p,From,To}))#writer{args=M}) end,
   {reply,{binary, #'Ack'{id=Id}},R,S}; % terminate to ws

info(Msg, R,S) ->
%   io:format("WS: ~p~n",[Msg]),
   {unknown,Msg,R,S}.

