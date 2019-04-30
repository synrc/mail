-module(chat_server).
-include_lib("n2o/include/n2o.hrl").
-include("message.hrl").
-compile(export_all).
-compile({parse_transform, bert_swift}).

info(#'Cut'{id=Id}, R, #cx{session = From} = S) ->
   kvx:cut(From,Id),
   {reply,{binary, #'Ack'{id=Id}},R,S};

info(#'Msg'{id=Id,feed=#'P2P'{to=To}}=Msg, R, S) ->
   kvx:append(Msg,To),
   n2o:send({client,To},{flush,Msg}),
   {reply,{binary, #'Ack'{id=Id}},R,S};

info(Msg, R,S) ->
   {unknown,Msg,R,S}.
