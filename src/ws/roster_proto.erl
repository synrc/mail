-module(roster_proto).
-include("message.hrl").
-include_lib("kvx/include/cursors.hrl").
-include_lib("n2o/include/n2o.hrl").
-compile(export_all).

info(#'Message'{from=From,to=To}=M, R,S) ->
   Reply = case kvx:get({p2p,From,To},M#'Message'.id) of
              {ok,_} -> {text, <<"ERR already saved.">>};
              {error,_} -> kvx:add((kvx:writer({p2p,From,To}))#writer{args=M}),
                           {bert, #'Ack'{id=M#'Message'.id,table='Message'}} end,
   {reply,Reply,R,S};

info(Msg, R,S) ->
   io:format("WS: ~p~n",[Msg]),
   {unknown,Msg,R,S}.

