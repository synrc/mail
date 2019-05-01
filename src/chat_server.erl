-module(chat_server).
-text('VNODE CHAT PROTOCOL').
-include_lib("n2o/include/n2o.hrl").
-include("ROSTER.hrl").
-compile(export_all).

info(#'Cut'{id=Id}, R, #cx{session = From} = S) ->
   kvx:cut(From,Id),
   {reply,{default, #'Ack'{lex=Id}},R,S};

info(#'Pub'{key=Id,adr=#'Adr'{dst={p2p,#'P2P'{dst=To}}}}=Msg, R, S) ->
   kvx:append(Msg,To),
   n2o:send({client,To},{flush,Msg}),
   {reply,{default, #'Ack'{lex=Id}},R,S};

info(Msg, R,S) ->
   {unknown,Msg,R,S}.
