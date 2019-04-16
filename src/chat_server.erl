-module(chat_server).
-copyright('2014—2019 (c) Synrc Research Center').
-include("message.hrl").
-include_lib("kvx/include/cursors.hrl").
-include_lib("n2o/include/n2o.hrl").
-compile(export_all).

% The Facebook database

info(#'Message'{from=From,to=To,id=Id}=M, R, #cx{state=kvx_rocks}=S) ->
   kvx:ensure(#writer{id={p2p,From,To}}),
   case kvx:get({p2p,From,To},Id) of
        {ok,_} -> skip;
        {error,_} -> kvx:add((kvx:writer({p2p,From,To}))#writer{args=M}),
                     n2o:send({client,From},{flush,M}),
                     n2o:send({client,To},{flush,M})
           end,    % for rocksdb version send the notify to WebSocket channels  
   {reply,{binary, #'Ack'{id=Id}},R,S};

% The WhatsApp database

info(#'Message'{from=From,to=To,id=Id}=M, R, #cx{state=kvx_mnesia}=S) ->
   kvx:ensure(#writer{id={p2p,From,To}}),
   case kvx:get('Message',Id) of
        {ok,_}    -> skip;
        {error,_} -> kvx:save(kvx:add((kvx:writer({p2p,From,To}))#writer{args=M})) end,
   {reply,{binary, #'Ack'{id=Id}},R,S};

info(Msg, R,S) ->
   {unknown,Msg,R,S}.

