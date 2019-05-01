-module(chat_ber).
-description('BER ASN.1 FORMATTER').
-include_lib("n2o/include/n2o.hrl").
-export([encode/1,decode/1]).

tag(Term) ->
    list_to_atom(string:to_lower(lists:concat([element(1,Term)]))).

encode(Term)       ->
    case 'ROSTER':encode('Msg', {tag(Term), Term}) of
         {ok,Bin} -> Bin;
         {error,_} -> <<>> end.

decode(Bin)        ->
    case 'ROSTER':decode('Msg', Bin) of
         {ok,{_,Msg}} ->
             io:format("DECODE OK: ~p~n",[Msg]), Msg;
         {error,{Reason,_}} ->
             io:format("DECODE ERROR ~p: ~p~n",
             [Reason,binary:part(Bin,0,erlang:min(size(Bin),16))]), [] end.
