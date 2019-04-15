-module(roster_vnode).
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

info(#'Message'{from=From,to=To}=M, R,S) ->
   Reply = case kvx:get({p2p,From,To},M#'Message'.id) of
              {ok,_} -> {text, <<"ERR already saved.">>};
              {error,_} -> kvx:add((kvx:writer({p2p,From,To}))#writer{args=M}),
                           {bert, #'Ack'{id=M#'Message'.id,table='Message'}} end,
   {reply,Reply,R,S};

info({flush,Text},R,S) ->
   {reply, {text, Text},R,S};

info(Msg, R,S) ->
   io:format("WS: ~p~n",[Msg]),
   {unknown,Msg,R,S}.

send(C,M) -> C ! M.

proc(init,#pi{name=Name}=Async) -> n2o:reg(Name), {ok,Async#pi{state=[]}};

proc({publish, C, Token, Request}, State = #pi{name=Server}) ->
    Ctx    = #cx { session= n2o:to_binary(Token), node=Server, client_pid=C, from=C },
    put(context, Ctx),
    Return = try case n2o_proto:info(Request,[],Ctx) of
             {reply,{_,      <<>>},_,_} -> skip;
             {reply,{text,   Text},_,_} -> {ok,send(C,{flush,Text})};
             {reply,{bert,   Term},_,_} -> {ok,send(C,n2o_bert:encode(Term))};
             {reply,{json,   Term},_,_} -> {ok,send(C,n2o_json:encode(Term))};
             {reply,{binary, Term},_,_} -> {ok,send(C,Term)};
             {reply,{default,Term},_,_} -> {ok,send(C,n2o:encode(Term))};
             {reply,{Encoder,Term},_,_} -> {ok,send(C,Encoder:encode(Term))};
                                  Reply -> {error,{"Invalid Return",Reply}} end
    catch _:_:S -> io:format("Catch:~p~n",[S]) end,
    {reply, Return, State};

proc(Unknown,#pi{name=Name}=Async) ->
    io:format("UNKNOWN ~p: ~p~n",[Name,Unknown]),
    {reply,{uknown,Unknown,0},Async}.
