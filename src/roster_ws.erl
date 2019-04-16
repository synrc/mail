-module(roster_ws).
-include("message.hrl").
-include_lib("kvx/include/cursors.hrl").
-include_lib("n2o/include/n2o.hrl").
-compile(export_all).

info({text,<<"N2O,",A/binary>>},R,S) ->
   n2o:reg({client,string:trim(binary_to_list(A))}),
   case kvx:get(writer,A) of
        {error,_} -> kvx:save(kvx:writer(A));
        {ok,_} -> skip end,
   {reply,{text,<<"USER ",A/binary>>},R,S#cx{session = A}};

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

info({text,<<"HIST",C/binary>>},R,S) ->
   case string:tokens(binary_to_list(C)," ") of
        [From,To] ->
           kvx:ensure(#writer{id={p2p,From,To}}),
           Fetch = (kvx:take((kvx:reader({p2p,From,To}))#reader{args=-1}))#reader.args,
           Res = string:join([ format_msg(M) || M <- Fetch ],"\n"),
           {reply,{text,<<(list_to_binary(format_chat({p2p,From,To})))/binary,
                          (list_to_binary(Res))/binary>>},R,S};
      _ -> {reply,{text,<<"ERROR in request.">>},R,S} end;

info({text,<<"SEEN",C/binary>>},R,S) ->
   Res = case string:tokens(binary_to_list(C)," ") of
        [From,To,Id] -> {reply,{text,<<"ERASED ",(bin(Id))/binary>>},R,S};
                   _ -> {reply,{text,<<"ERROR in request.">>},R,S} end;

info({flush,#'Message'{}=M},R,S)  -> {reply, {text,<<"NOTIFY ",(list_to_binary(format_msg(M)))/binary>>},R,S};
info(#'Ack'{id=Key}, R,S) -> {reply, {text,<<"ACK ",(bin(Key))/binary>>},R,S};
info({flush,Text},R,S)    -> {reply, {text,Text},R,S};
info(Msg, R,S)            -> {unknown,Msg,R,S}.

bin(Key) ->
  list_to_binary(io_lib:format("~p",[Key])).

format_msg(#'Message'{id=Id,from=From,to=To,files=Files}) ->
  P = case Files of [#'File'{payload=X}] -> X; _ -> [] end,
  io_lib:format("~s:~s:~s:~s",[From,To,Id,P]).

format_chat({p2p,From,To}) ->
  io_lib:format("CHAT ~s ~s~n",[From,To]).

user(Id) ->
  case kvx:get(writer,n2o:to_binary(Id)) of
       {ok,_} -> true;
       {error,_} -> false end.
