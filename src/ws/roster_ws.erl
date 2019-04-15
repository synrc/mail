-module(roster_ws).
-include("message.hrl").
-include_lib("kvx/include/cursors.hrl").
-include_lib("n2o/include/n2o.hrl").
-compile(export_all).

info({text,<<"N2O,",A/binary>>},R,S) ->
   n2o:reg({client,A}),
   case kvx:get(writer,A) of
        {error,_} -> kvx:save(kvx:writer(A));
        {ok,_} -> skip end,
   {reply,{text,<<"USER ",A/binary>>},R,S#cx{session = A}};

info({text,<<"HIST ",C/binary>>},R,S) ->
   case string:tokens(binary_to_list(C)," ") of
        [From,To] ->
           kvx:ensure(#writer{id={p2p,From,To}}),
           Res = string:join([ io_lib:format("~s:~s:~s",[From,To,P])
             || #'Message'{from=From,to=To,files=[#'File'{payload=P}]}
             <- (kvx:take((kvx:reader({p2p,From,To}))#reader{args=-1}))#reader.args ],"\n"),
           {reply,{text,<<"History:\n",(list_to_binary(Res))/binary>>},R,S};
      _ -> {reply,{text,<<"ERROR in request.">>},R,S} end;

info({text,<<"MSG ",C/binary>>},R,#cx{session = Sid}=S) ->
   case string:tokens(binary_to_list(C)," ") of
        [From,To,Id|Rest] ->
           Key = case Id of "0" -> kvx:seq([],[]); I -> I end,
           Msg = #'Message'{id=Key,from=From,to=To,files=[#'File'{payload=string:join(Rest," ")}]},
           Res = case user(From) andalso user(To) of
                 false -> <<"ERR user doesn't exist.">>;
                 true  -> {ring,N} = n2o_ring:lookup({p2p,From,To}),
                          n2o:send({server,N},{publish,self(),Sid,Msg}),
                          <<"SENT ",(bin(Key))/binary>> end,
            {reply, {text, Res},R,S};
       _ -> {reply, {text, <<"ERROR in request.">>},R,S} end;

info({flush,Text},R,S)    -> {reply, {text, Text},R,S};
info(#'Ack'{id=Key}, R,S) -> {reply, {text,<<"ACK ",(bin(Key))/binary>>},R,S};
info(Msg, R,S)            -> {unknown,Msg,R,S}.
bin(Key)                  -> list_to_binary(io_lib:format("~p",[Key])).

user(Id) ->
  case kvx:get(writer,n2o:to_binary(Id)) of
       {ok,_} -> true;
       {error,_} -> false end.
