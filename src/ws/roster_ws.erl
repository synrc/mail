-module(roster_ws).
-include("message.hrl").
-include_lib("kvx/include/cursors.hrl").
-include_lib("n2o/include/n2o.hrl").
-compile(export_all).

user(Id) ->
   case kvx:get(writer,n2o:to_binary(Id)) of
        {ok,_} -> true;
        {error,_} -> false end.

info({text,<<"N2O,",A/binary>>},R,S) ->
   n2o:reg({client,A}),
   kvx:writer(A),
   {reply,{text,<<"USER ",A/binary>>},R,S#cx{session = A}};

info({text,<<"MSG ",C/binary>>},R,#cx{session = Sid}=S) ->
   [From,To,Payload|TS] = string:tokens(binary_to_list(C)," "),
   Key = case TS of [X] -> X; _ -> kvx:seq([],[]) end,
   Msg = #'Message'{id=Key,from=From,to=To,files=[#'File'{payload=Payload}]},
   Res = case user(From) andalso user(To) of
         false -> <<"ERR unexistent user.">>;
         true  -> {ring,N} = n2o_ring:lookup({p2p,From,To}),
                  {ok,Ack} = n2o:send({server,N},{publish,self(),Sid,Msg}),
                  <<"ACK ",(list_to_binary(io_lib:format("~p",[Ack])))/binary>> end,
   {reply, {text, Res},R,S};

info({flush,Text},R,S) ->
   {reply, {text, Text},R,S};

info(Msg, R,S) ->
   io:format("WS: ~p~n",[Msg]),
   {unknown,Msg,R,S}.

