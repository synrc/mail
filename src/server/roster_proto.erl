-module(roster_proto).
-include("message.hrl").
-include_lib("kvx/include/cursors.hrl").
-include_lib("n2o/include/n2o.hrl").
-compile(export_all).

info(#'Message'{from=From,to=To,id=Id}=M, R, #cx{state=kvx_rocks}=S) ->
   io:format("ROCKSDB write.~n"),
   kvx:ensure(#writer{id={p2p,From,To}}),
   case kvx:get({p2p,From,To},Id) of
        {ok,_} -> skip;
        {error,_} -> kvx:add((kvx:writer({p2p,From,To}))#writer{args=M}) end,
   {reply,{binary, #'Ack'{id=Id}},R,S};

info(#'Message'{from=From,to=To,id=Id}=M, R, #cx{state=kvx_mnesia}=S) ->
   io:format("MNESIA write.~n"),
   kvx:ensure(#writer{id={p2p,From,To}}),
   case kvx:get('Message',Id) of
        {ok,_}    -> skip;
        {error,_} -> kvx:save(kvx:add((kvx:writer({p2p,From,To}))#writer{args=M})) end,
   {reply,{binary, #'Ack'{id=Id}},R,S};

info(Msg, R,S) ->
%   io:format("WS: ~p~n",[Msg]),
   {unknown,Msg,R,S}.

