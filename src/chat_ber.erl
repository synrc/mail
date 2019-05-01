-module(chat_ber).
-text('BERT/BER/ASN.1 FORMATTER').
-text('BER/BERT CHAT PROTOCOL').
-include_lib("kvx/include/cursors.hrl").
-include("ROSTER.hrl").
-include_lib("n2o/include/n2o.hrl").
-export([encode/1,decode/1,info/3]).

tag(Term) ->
    list_to_atom(string:to_lower(lists:concat([element(1,Term)]))).

encode(Term)       ->
    case 'ROSTER':encode('Msg', {tag(Term), Term}) of
         {ok,Bin} -> Bin;
         {error,_} -> term_to_binary(Term) end.

decode(Bin)        ->
    case 'ROSTER':decode('Msg', Bin) of
         {ok,{_,Msg}} -> Msg;
         {error,_} ->
             io:format("DECODE ERROR ~p~n",[Bin]),
             try binary_to_term(Bin,[safe]) catch _:_ -> [] end end.

info(#'N2O'{tok=Key}=Msg, R, S) ->
    A = string:trim(binary_to_list(Key)),
    n2o:reg({client,A}),
    kvx:ensure(#writer{id=A}),
    io:format("BER N2O: ~p~n",[Msg]),
    {reply, {chat:fmt(), Msg},R,S#cx{session = A}};

info(#'Pub'{adr=#'Adr'{src=From,dst={p2p,#'P2P'{dst=To}}}}=Msg, R, S) ->
    {ring,N} = n2o_ring:lookup(To),
    n2o:send({server,N},{publish,self(),From,Msg}),
    io:format("BER PUB: ~p~n",[Msg]),
    {reply, {text, <<>>},R,S};

info(#'Ack'{lex= <<>>}=Msg, R, #cx{session = From} = S) ->
    io:format("BER BOX: ~p~n",[Msg]),
    kvx:ensure(#writer{id=From}),
    Fetch = (kvx:take((kvx:reader(From))#reader{args=-1}))#reader.args,
    Res = "LIST\n" ++ string:join([ chat:format_msg(M) || M <- lists:reverse(Fetch) ],"\n"),
    {reply,{text,<<(list_to_binary(Res))/binary>>},R,S};

info(#'Ack'{lex=Key}=Ack, R,S) ->
    io:format("BER ACK: ~p~n",[Ack]),
    {reply, {text,<<"ACK ",(chat:bin(Key))/binary>>},R,S};

info(Msg, R,S) ->
    {unknown,Msg,R,S}.
