-module(chat_server).
-text('VNODE CHAT PROTOCOL').
-include_lib("n2o/include/n2o.hrl").
-include("ROSTER.hrl").
-compile(export_all).

info(#'Cut'{id=Id}=Msg, R, #cx{session = From} = S) ->
   io:format("SERVER: ~p~n",[Msg]),
   kvx:cut(From,Id),
   {reply,{default, #'Ack'{lex=Id}},R,S};

info(#'Pub'{key=Id,adr=#'Adr'{dst={p2p,#'P2P'{dst=To}}}}=Msg, R, S) ->
   io:format("SERVER ~p: ~p~n",[To,Msg]),
   kvx:append(Msg,To),
   n2o:send({client,To},{flush,Msg}),
   {reply,{binary, #'Ack'{lex=Id}},R,S};

info(Msg, R,S) -> {unknown,Msg,R,S}.
