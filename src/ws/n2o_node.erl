-module(n2o_node).
-include("message.hrl").
-include_lib("n2o/include/n2o.hrl").
-copyright('Vladimir Kirillov').
-compile(export_all).

info({text,<<"N2O,",A/binary>>},R,S) ->
   n2o:reg({client,A}),
   {reply,{text,<<"USER ",A/binary>>},R,S#cx{session = A}};

info({text,<<"MSG ",C/binary>>},R,#cx{session = Sid}=S) ->
   _Tokens = string:tokens(binary_to_list(C)," "),
   Msg = #'Message'{client_id=kvx:seq([],[]),files=[#'File'{payload=C}]},
   {ring,N} = n2o_ring:lookup(Msg),
   {ok,Res} = n2o:send({server,N},{publish,self(),Sid,Msg}),
   Delivered = list_to_binary(io_lib:format("~p",[Res])),
   {reply, {text, <<"ACK ",Delivered/binary>>},R,S};

info(#'Message'{}=M, _, #cx{node=Server}) ->
    io:format("NODE ~p: ~p~n",[Server,M]),
   ok;

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
