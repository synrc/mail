-module(chat_client).
-include("message.hrl").
-include_lib("kvx/include/cursors.hrl").
-include_lib("n2o/include/n2o.hrl").
-compile(export_all).

info({text,<<"N2O",X/binary>>},R,S) -> % auth
   A = string:trim(binary_to_list(X)),
   n2o:reg({client,A}),
   kvx:ensure(#writer{id=A}),
   {reply,{text,<<(list_to_binary("USER " ++ A))/binary>>},R,S#cx{session = A}};

info({text,<<"SEND",_/binary>>},R,#cx{session = []}=S) ->
   {reply, {text, <<"Please login with N2O. Try HELP.">>},R,S};

info({text,<<"SEND",X/binary>>},R,#cx{session = From}=S) -> % send message
   case string:tokens(string:trim(binary_to_list(X))," ") of
        [To|Rest] ->
           Key = kvx:seq([],[]),
           Msg = #'Msg'{id=Key,feed=#'P2P'{from=From,to=To},files=[#'Bin'{payload=string:join(Rest," ")}]},
           Res = case user(To) of
                 false -> <<"ERROR user doesn't exist.">>;
                 true  -> % here is feed consistency happens
                          {ring,N} = n2o_ring:lookup(To),
                          n2o:send({server,N},{publish,self(),From,Msg}),
                          <<>> end,
            {reply, {text, Res},R,S};
       _ -> {reply, {text, <<"ERROR in request.">>},R,S} end;

info({text,<<"BOX">>},R,#cx{session = From}=S) -> % print the feed
   kvx:ensure(#writer{id=From}),
   Fetch = (kvx:take((kvx:reader(From))#reader{args=-1}))#reader.args,
   Res = "LIST\n" ++ string:join([ format_msg(M) || M <- lists:reverse(Fetch) ],"\n"),
   {reply,{text,<<(list_to_binary(Res))/binary>>},R,S};

info({text,<<"HELP">>},R,S) -> % erase the feed by SEEN command
   {reply, {text,<<"N2O <user>\n| SEND <user> <msg>\n| BOX\n| CUT <id>.">>},R,S};

info({text,<<"CUT",X/binary>>},R,#cx{session = From}=S) -> % erase the feed by SEEN command
   case string:tokens(string:trim(binary_to_list(X))," ") of
        [Id] -> case kvx:cut(From,Id) of
                             {ok,Count} -> {reply,{text,<<"ERASED ",(bin(Count))/binary>>},R,S};
                              {error,_} -> {reply,{text,<<"NOT FOUND ">>},R,S} end;
                                      _ -> {reply,{text,<<"ERROR in request.">>},R,S} end;

info({flush,#'Msg'{}=M},R,S)  -> {reply,
   {text,<<"NOTIFY ",(list_to_binary(format_msg(M)))/binary>>},R,S};

info(#'Ack'{id=Key}, R,S) -> {reply, {text,<<"ACK ",(bin(Key))/binary>>},R,S};
info({flush,Text},R,S)    -> {reply, {text,Text},R,S};
info({text,_}, R,S)       -> {reply, {text,<<"Try HELP">>},R,S};
info(Msg, R,S)            -> {unknown,Msg,R,S}.

bin(Key) -> list_to_binary(io_lib:format("~p",[Key])).
user(Id) -> case kvx:get(writer,Id) of {ok,_} -> true; {error,_} -> false end.
format_msg(#'Msg'{id=Id,feed=#'P2P'{from=From,to=To},files=Files}) ->
  P = case Files of [#'Bin'{payload=X}] -> X; _ -> [] end,
  io_lib:format("~s:~s:~s:~s",[From,To,Id,P]).

