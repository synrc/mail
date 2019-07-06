-module(chat_txt).
-text('TXT CHAT PROTOCOL').
-include("ROSTER.hrl").
-include_lib("kvs/include/cursors.hrl").
-include_lib("n2o/include/n2o.hrl").
-compile(export_all).

info({text,<<"N2O",X/binary>>},R,S) -> % auth
    A = string:trim(binary_to_list(X)),
    Key = "/chat/" ++ A,
    n2o:reg({client,Key}),
    kvs:ensure(#writer{id=Key}),
    io:format("TXT N2O: ~p~n",[A]),
    {reply,{text,<<(list_to_binary("USER " ++ A))/binary>>},R,S#cx{session = A}};

info({text,<<"SEND",_/binary>>},R,#cx{session = []}=S) ->
    {reply, {text, <<"Please login with N2O. Try HELP.">>},R,S};

info({text,<<"SEND",X/binary>>},R,#cx{session = From}=S) -> % send message
    case string:tokens(string:trim(binary_to_list(X))," ") of
        [To|Rest] ->
           Key = kvs:seq([],[]),
           Msg = #'Pub'{key=Key,adr=#'Adr'{src=From,dst={p2p,#'P2P'{dst=To}}},bin=iolist_to_binary(string:join(Rest," "))},
           Res = case chat:user(To) of
                 false -> <<"ERROR user doesn't exist.">>;
                 true  -> n2o_ring:send(ws, {publish, self(), From, Msg}), <<>> end,
            {reply, {text, Res},R,S};
       _ -> {reply, {text, <<"ERROR in request.">>},R,S} end;

info({text,<<"BOX">>},R,#cx{session = From}=S) -> % print the feed
    Key = "/chat/"++From,
    kvs:ensure(#writer{id=Key}),
    Fetch = kvs:head(Key,10),
    Res = "LIST\n" ++ string:join([ chat:format_msg(M) || M <- lists:reverse(Fetch) ],"\n"),
    {reply,{text,<<(list_to_binary(Res))/binary>>},R,S};

info({text,<<"HELP">>},R,S) -> % erase the feed by SEEN command
    {reply, {text,<<"N2O <user>\n| SEND <user> <msg>\n| BOX\n| CUT <id>.">>},R,S};

info({text,<<"CUT",X/binary>>},R,#cx{session = From}=S) -> % erase the feed by SEEN command
    case string:tokens(string:trim(binary_to_list(X))," ") of
         [Id] -> case kvs:cut("/chat/"++From,Id) of
                              {ok,Count} -> {reply,{text,<<"ERASED ",(chat:bin(Count))/binary>>},R,S};
                               {error,_} -> {reply,{text,<<"NOT FOUND ">>},R,S} end;
                                       _ -> {reply,{text,<<"ERROR in request.">>},R,S} end;

info({flush,#'Pub'{}=M},R,S)  ->
    io:format("FLUSH~n"),
    {reply, {text,<<"NOTIFY ",(list_to_binary(chat:format_msg(M)))/binary>>},R,S};

info({flush,Text},R,S)     -> {reply, {text,Text},R,S};
info({text,_}, R,S)        -> {reply, {text,<<"Try HELP">>},R,S};
info(Msg, R,S)             -> {unknown,Msg,R,S}.

