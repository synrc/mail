%% Generated by the Erlang ASN.1 BER_V2-compiler version, utilizing bit-syntax:4.0.4
%% Purpose: encoder and decoder to the types in mod ROSTER

-module('ROSTER').
-compile(nowarn_unused_vars).
-dialyzer(no_improper_lists).
-include("ROSTER.hrl").
-asn1_info([{vsn,'4.0.4'},
            {module,'ROSTER'},
            {options,[warnings,ber,errors,
 {cwd,"/Users/maxim/depot/synrc/apps/erlang/chat/src"},
 {outdir,"/Users/maxim/depot/synrc/apps/erlang/chat/src"},
 {i,"."},
 {i,"/Users/maxim/depot/synrc/apps/erlang/chat/src"}]}]).

-export([encoding_rule/0,bit_string_format/0,
         legacy_erlang_types/0]).
-export(['dialyzer-suppressions'/1]).
-export([
'enc_P2P'/2,
'enc_MUC'/2,
'enc_Adr'/2,
'enc_Pub'/2,
'enc_Sub'/2,
'enc_Cut'/2,
'enc_N2O'/2,
'enc_Ack'/2,
'enc_Nak'/2,
'enc_Msg'/2
]).

-export([
'dec_P2P'/2,
'dec_MUC'/2,
'dec_Adr'/2,
'dec_Pub'/2,
'dec_Sub'/2,
'dec_Cut'/2,
'dec_N2O'/2,
'dec_Ack'/2,
'dec_Nak'/2,
'dec_Msg'/2
]).

-export([info/0]).


-export([encode/2,decode/2]).

encoding_rule() -> ber.

bit_string_format() -> bitstring.

legacy_erlang_types() -> false.

encode(Type, Data) ->
try iolist_to_binary(element(1, encode_disp(Type, Data))) of
  Bytes ->
    {ok,Bytes}
  catch
    Class:Exception when Class =:= error; Class =:= exit ->
      case Exception of
        {error,Reason}=Error ->
          Error;
        Reason ->
         {error,{asn1,Reason}}
      end
end.

decode(Type,Data) ->
try decode_disp(Type, element(1, ber_decode_nif(Data))) of
  Result ->
    {ok,Result}
  catch
    Class:Exception when Class =:= error; Class =:= exit ->
      case Exception of
        {error,Reason}=Error ->
          Error;
        Reason ->
         {error,{asn1,Reason}}
      end
end.

encode_disp('P2P',Data) -> 'enc_P2P'(Data);
encode_disp('MUC',Data) -> 'enc_MUC'(Data);
encode_disp('Adr',Data) -> 'enc_Adr'(Data);
encode_disp('Pub',Data) -> 'enc_Pub'(Data);
encode_disp('Sub',Data) -> 'enc_Sub'(Data);
encode_disp('Cut',Data) -> 'enc_Cut'(Data);
encode_disp('N2O',Data) -> 'enc_N2O'(Data);
encode_disp('Ack',Data) -> 'enc_Ack'(Data);
encode_disp('Nak',Data) -> 'enc_Nak'(Data);
encode_disp('Msg',Data) -> 'enc_Msg'(Data);
encode_disp(Type,_Data) -> exit({error,{asn1,{undefined_type,Type}}}).


decode_disp('P2P',Data) -> 'dec_P2P'(Data);
decode_disp('MUC',Data) -> 'dec_MUC'(Data);
decode_disp('Adr',Data) -> 'dec_Adr'(Data);
decode_disp('Pub',Data) -> 'dec_Pub'(Data);
decode_disp('Sub',Data) -> 'dec_Sub'(Data);
decode_disp('Cut',Data) -> 'dec_Cut'(Data);
decode_disp('N2O',Data) -> 'dec_N2O'(Data);
decode_disp('Ack',Data) -> 'dec_Ack'(Data);
decode_disp('Nak',Data) -> 'dec_Nak'(Data);
decode_disp('Msg',Data) -> 'dec_Msg'(Data);
decode_disp(Type,_Data) -> exit({error,{asn1,{undefined_type,Type}}}).




info() ->
   case ?MODULE:module_info(attributes) of
     Attributes when is_list(Attributes) ->
       case lists:keyfind(asn1_info, 1, Attributes) of
         {_,Info} when is_list(Info) ->
           Info;
         _ ->
           []
       end;
     _ ->
       []
   end.


%%================================
%%  P2P
%%================================
'enc_P2P'(Val) ->
    'enc_P2P'(Val, [<<48>>]).

'enc_P2P'(Val, TagIn) ->
{_,Cindex1} = Val,

%%-------------------------------------------------
%% attribute dst(1) with type OCTET STRING
%%-------------------------------------------------
   {EncBytes1,EncLen1} = encode_restricted_string(Cindex1, [<<4>>]),

   BytesSoFar = [EncBytes1],
LenSoFar = EncLen1,
encode_tags(TagIn, BytesSoFar, LenSoFar).


'dec_P2P'(Tlv) ->
   'dec_P2P'(Tlv, [16]).

'dec_P2P'(Tlv, TagIn) ->
   %%-------------------------------------------------
   %% decode tag and length 
   %%-------------------------------------------------
Tlv1 = match_tags(Tlv, TagIn),

%%-------------------------------------------------
%% attribute dst(1) with type OCTET STRING
%%-------------------------------------------------
[V1|Tlv2] = Tlv1, 
Term1 = decode_octet_string(V1, [4]),

case Tlv2 of
[] -> true;_ -> exit({error,{asn1, {unexpected,Tlv2}}}) % extra fields not allowed
end,
   {'P2P', Term1}.



%%================================
%%  MUC
%%================================
'enc_MUC'(Val) ->
    'enc_MUC'(Val, [<<48>>]).

'enc_MUC'(Val, TagIn) ->
{_,Cindex1} = Val,

%%-------------------------------------------------
%% attribute dst(1) with type OCTET STRING
%%-------------------------------------------------
   {EncBytes1,EncLen1} = encode_restricted_string(Cindex1, [<<4>>]),

   BytesSoFar = [EncBytes1],
LenSoFar = EncLen1,
encode_tags(TagIn, BytesSoFar, LenSoFar).


'dec_MUC'(Tlv) ->
   'dec_MUC'(Tlv, [16]).

'dec_MUC'(Tlv, TagIn) ->
   %%-------------------------------------------------
   %% decode tag and length 
   %%-------------------------------------------------
Tlv1 = match_tags(Tlv, TagIn),

%%-------------------------------------------------
%% attribute dst(1) with type OCTET STRING
%%-------------------------------------------------
[V1|Tlv2] = Tlv1, 
Term1 = decode_octet_string(V1, [4]),

case Tlv2 of
[] -> true;_ -> exit({error,{asn1, {unexpected,Tlv2}}}) % extra fields not allowed
end,
   {'MUC', Term1}.



%%================================
%%  Adr
%%================================
'enc_Adr'(Val) ->
    'enc_Adr'(Val, [<<48>>]).

'enc_Adr'(Val, TagIn) ->
{_,Cindex1, Cindex2} = Val,

%%-------------------------------------------------
%% attribute src(1) with type OCTET STRING
%%-------------------------------------------------
   {EncBytes1,EncLen1} = encode_restricted_string(Cindex1, [<<4>>]),

%%-------------------------------------------------
%% attribute dst(2) with type CHOICE
%%-------------------------------------------------
   {EncBytes2,EncLen2} = 'enc_Adr_dst'(Cindex2, []),

   BytesSoFar = [EncBytes1, EncBytes2],
LenSoFar = EncLen1 + EncLen2,
encode_tags(TagIn, BytesSoFar, LenSoFar).



%%================================
%%  Adr_dst
%%================================
'enc_Adr_dst'(Val, TagIn) ->
      {EncBytes,EncLen} = case element(1,Val) of
      muc ->
         'enc_MUC'(element(2,Val), [<<160>>]);
      p2p ->
         'enc_P2P'(element(2,Val), [<<161>>]);
      Else -> 
         exit({error,{asn1,{invalid_choice_type,Else}}})
   end,

encode_tags(TagIn, EncBytes, EncLen).




'dec_Adr'(Tlv) ->
   'dec_Adr'(Tlv, [16]).

'dec_Adr'(Tlv, TagIn) ->
   %%-------------------------------------------------
   %% decode tag and length 
   %%-------------------------------------------------
Tlv1 = match_tags(Tlv, TagIn),

%%-------------------------------------------------
%% attribute src(1) with type OCTET STRING
%%-------------------------------------------------
[V1|Tlv2] = Tlv1, 
Term1 = decode_octet_string(V1, [4]),

%%-------------------------------------------------
%% attribute dst(2) with type CHOICE
%%-------------------------------------------------
[V2|Tlv3] = Tlv2, 
Term2 = 'dec_Adr_dst'(V2, []),

case Tlv3 of
[] -> true;_ -> exit({error,{asn1, {unexpected,Tlv3}}}) % extra fields not allowed
end,
   {'Adr', Term1, Term2}.

'dec_Adr_dst'(Tlv, TagIn) ->
Tlv1 = match_tags(Tlv, TagIn),
case (case Tlv1 of [CtempTlv1] -> CtempTlv1; _ -> Tlv1 end) of

%% 'muc'
    {131072, V1} -> 
        {muc, 'dec_MUC'(V1, [])};


%% 'p2p'
    {131073, V1} -> 
        {p2p, 'dec_P2P'(V1, [])};

      Else -> 
         exit({error,{asn1,{invalid_choice_tag,Else}}})
   end
.


%%================================
%%  Pub
%%================================
'enc_Pub'(Val) ->
    'enc_Pub'(Val, [<<48>>]).

'enc_Pub'(Val, TagIn) ->
{_,Cindex1, Cindex2, Cindex3, Cindex4} = Val,

%%-------------------------------------------------
%% attribute key(1) with type OCTET STRING
%%-------------------------------------------------
   {EncBytes1,EncLen1} = encode_restricted_string(Cindex1, [<<4>>]),

%%-------------------------------------------------
%% attribute adr(2)   External ROSTER:Adr
%%-------------------------------------------------
   {EncBytes2,EncLen2} = 'enc_Adr'(Cindex2, [<<48>>]),

%%-------------------------------------------------
%% attribute tag(3) with type OCTET STRING
%%-------------------------------------------------
   {EncBytes3,EncLen3} = encode_restricted_string(Cindex3, [<<4>>]),

%%-------------------------------------------------
%% attribute bin(4) with type OCTET STRING
%%-------------------------------------------------
   {EncBytes4,EncLen4} = encode_restricted_string(Cindex4, [<<4>>]),

   BytesSoFar = [EncBytes1, EncBytes2, EncBytes3, EncBytes4],
LenSoFar = EncLen1 + EncLen2 + EncLen3 + EncLen4,
encode_tags(TagIn, BytesSoFar, LenSoFar).


'dec_Pub'(Tlv) ->
   'dec_Pub'(Tlv, [16]).

'dec_Pub'(Tlv, TagIn) ->
   %%-------------------------------------------------
   %% decode tag and length 
   %%-------------------------------------------------
Tlv1 = match_tags(Tlv, TagIn),

%%-------------------------------------------------
%% attribute key(1) with type OCTET STRING
%%-------------------------------------------------
[V1|Tlv2] = Tlv1, 
Term1 = decode_octet_string(V1, [4]),

%%-------------------------------------------------
%% attribute adr(2)   External ROSTER:Adr
%%-------------------------------------------------
[V2|Tlv3] = Tlv2, 
Term2 = 'dec_Adr'(V2, [16]),

%%-------------------------------------------------
%% attribute tag(3) with type OCTET STRING
%%-------------------------------------------------
[V3|Tlv4] = Tlv3, 
Term3 = decode_octet_string(V3, [4]),

%%-------------------------------------------------
%% attribute bin(4) with type OCTET STRING
%%-------------------------------------------------
[V4|Tlv5] = Tlv4, 
Term4 = decode_octet_string(V4, [4]),

case Tlv5 of
[] -> true;_ -> exit({error,{asn1, {unexpected,Tlv5}}}) % extra fields not allowed
end,
   {'Pub', Term1, Term2, Term3, Term4}.



%%================================
%%  Sub
%%================================
'enc_Sub'(Val) ->
    'enc_Sub'(Val, [<<48>>]).

'enc_Sub'(Val, TagIn) ->
{_,Cindex1, Cindex2} = Val,

%%-------------------------------------------------
%% attribute key(1) with type OCTET STRING
%%-------------------------------------------------
   {EncBytes1,EncLen1} = encode_restricted_string(Cindex1, [<<4>>]),

%%-------------------------------------------------
%% attribute adr(2)   External ROSTER:Adr
%%-------------------------------------------------
   {EncBytes2,EncLen2} = 'enc_Adr'(Cindex2, [<<48>>]),

   BytesSoFar = [EncBytes1, EncBytes2],
LenSoFar = EncLen1 + EncLen2,
encode_tags(TagIn, BytesSoFar, LenSoFar).


'dec_Sub'(Tlv) ->
   'dec_Sub'(Tlv, [16]).

'dec_Sub'(Tlv, TagIn) ->
   %%-------------------------------------------------
   %% decode tag and length 
   %%-------------------------------------------------
Tlv1 = match_tags(Tlv, TagIn),

%%-------------------------------------------------
%% attribute key(1) with type OCTET STRING
%%-------------------------------------------------
[V1|Tlv2] = Tlv1, 
Term1 = decode_octet_string(V1, [4]),

%%-------------------------------------------------
%% attribute adr(2)   External ROSTER:Adr
%%-------------------------------------------------
[V2|Tlv3] = Tlv2, 
Term2 = 'dec_Adr'(V2, [16]),

case Tlv3 of
[] -> true;_ -> exit({error,{asn1, {unexpected,Tlv3}}}) % extra fields not allowed
end,
   {'Sub', Term1, Term2}.



%%================================
%%  Cut
%%================================
'enc_Cut'(Val) ->
    'enc_Cut'(Val, [<<48>>]).

'enc_Cut'(Val, TagIn) ->
{_,Cindex1} = Val,

%%-------------------------------------------------
%% attribute id(1) with type OCTET STRING
%%-------------------------------------------------
   {EncBytes1,EncLen1} = encode_restricted_string(Cindex1, [<<4>>]),

   BytesSoFar = [EncBytes1],
LenSoFar = EncLen1,
encode_tags(TagIn, BytesSoFar, LenSoFar).


'dec_Cut'(Tlv) ->
   'dec_Cut'(Tlv, [16]).

'dec_Cut'(Tlv, TagIn) ->
   %%-------------------------------------------------
   %% decode tag and length 
   %%-------------------------------------------------
Tlv1 = match_tags(Tlv, TagIn),

%%-------------------------------------------------
%% attribute id(1) with type OCTET STRING
%%-------------------------------------------------
[V1|Tlv2] = Tlv1, 
Term1 = decode_octet_string(V1, [4]),

case Tlv2 of
[] -> true;_ -> exit({error,{asn1, {unexpected,Tlv2}}}) % extra fields not allowed
end,
   {'Cut', Term1}.



%%================================
%%  N2O
%%================================
'enc_N2O'(Val) ->
    'enc_N2O'(Val, [<<48>>]).

'enc_N2O'(Val, TagIn) ->
{_,Cindex1} = Val,

%%-------------------------------------------------
%% attribute tok(1) with type OCTET STRING
%%-------------------------------------------------
   {EncBytes1,EncLen1} = encode_restricted_string(Cindex1, [<<4>>]),

   BytesSoFar = [EncBytes1],
LenSoFar = EncLen1,
encode_tags(TagIn, BytesSoFar, LenSoFar).


'dec_N2O'(Tlv) ->
   'dec_N2O'(Tlv, [16]).

'dec_N2O'(Tlv, TagIn) ->
   %%-------------------------------------------------
   %% decode tag and length 
   %%-------------------------------------------------
Tlv1 = match_tags(Tlv, TagIn),

%%-------------------------------------------------
%% attribute tok(1) with type OCTET STRING
%%-------------------------------------------------
[V1|Tlv2] = Tlv1, 
Term1 = decode_octet_string(V1, [4]),

case Tlv2 of
[] -> true;_ -> exit({error,{asn1, {unexpected,Tlv2}}}) % extra fields not allowed
end,
   {'N2O', Term1}.



%%================================
%%  Ack
%%================================
'enc_Ack'(Val) ->
    'enc_Ack'(Val, [<<48>>]).

'enc_Ack'(Val, TagIn) ->
{_,Cindex1} = Val,

%%-------------------------------------------------
%% attribute lex(1) with type OCTET STRING
%%-------------------------------------------------
   {EncBytes1,EncLen1} = encode_restricted_string(Cindex1, [<<4>>]),

   BytesSoFar = [EncBytes1],
LenSoFar = EncLen1,
encode_tags(TagIn, BytesSoFar, LenSoFar).


'dec_Ack'(Tlv) ->
   'dec_Ack'(Tlv, [16]).

'dec_Ack'(Tlv, TagIn) ->
   %%-------------------------------------------------
   %% decode tag and length 
   %%-------------------------------------------------
Tlv1 = match_tags(Tlv, TagIn),

%%-------------------------------------------------
%% attribute lex(1) with type OCTET STRING
%%-------------------------------------------------
[V1|Tlv2] = Tlv1, 
Term1 = decode_octet_string(V1, [4]),

case Tlv2 of
[] -> true;_ -> exit({error,{asn1, {unexpected,Tlv2}}}) % extra fields not allowed
end,
   {'Ack', Term1}.



%%================================
%%  Nak
%%================================
'enc_Nak'(Val) ->
    'enc_Nak'(Val, [<<48>>]).

'enc_Nak'(Val, TagIn) ->
{_,Cindex1} = Val,

%%-------------------------------------------------
%% attribute key(1) with type OCTET STRING
%%-------------------------------------------------
   {EncBytes1,EncLen1} = encode_restricted_string(Cindex1, [<<4>>]),

   BytesSoFar = [EncBytes1],
LenSoFar = EncLen1,
encode_tags(TagIn, BytesSoFar, LenSoFar).


'dec_Nak'(Tlv) ->
   'dec_Nak'(Tlv, [16]).

'dec_Nak'(Tlv, TagIn) ->
   %%-------------------------------------------------
   %% decode tag and length 
   %%-------------------------------------------------
Tlv1 = match_tags(Tlv, TagIn),

%%-------------------------------------------------
%% attribute key(1) with type OCTET STRING
%%-------------------------------------------------
[V1|Tlv2] = Tlv1, 
Term1 = decode_octet_string(V1, [4]),

case Tlv2 of
[] -> true;_ -> exit({error,{asn1, {unexpected,Tlv2}}}) % extra fields not allowed
end,
   {'Nak', Term1}.



%%================================
%%  Msg
%%================================
'enc_Msg'(Val) ->
    'enc_Msg'(Val, []).

'enc_Msg'(Val, TagIn) ->
   {EncBytes,EncLen} = case element(1,Val) of
      n2o ->
         'enc_N2O'(element(2,Val), [<<160>>]);
      ack ->
         'enc_Ack'(element(2,Val), [<<161>>]);
      nak ->
         'enc_Nak'(element(2,Val), [<<162>>]);
      pub ->
         'enc_Pub'(element(2,Val), [<<163>>]);
      sub ->
         'enc_Sub'(element(2,Val), [<<164>>]);
      cut ->
         'enc_Cut'(element(2,Val), [<<165>>]);
      Else -> 
         exit({error,{asn1,{invalid_choice_type,Else}}})
   end,

encode_tags(TagIn, EncBytes, EncLen).




'dec_Msg'(Tlv) ->
   'dec_Msg'(Tlv, []).

'dec_Msg'(Tlv, TagIn) ->
Tlv1 = match_tags(Tlv, TagIn),
case (case Tlv1 of [CtempTlv1] -> CtempTlv1; _ -> Tlv1 end) of

%% 'n2o'
    {131072, V1} -> 
        {n2o, 'dec_N2O'(V1, [])};


%% 'ack'
    {131073, V1} -> 
        {ack, 'dec_Ack'(V1, [])};


%% 'nak'
    {131074, V1} -> 
        {nak, 'dec_Nak'(V1, [])};


%% 'pub'
    {131075, V1} -> 
        {pub, 'dec_Pub'(V1, [])};


%% 'sub'
    {131076, V1} -> 
        {sub, 'dec_Sub'(V1, [])};


%% 'cut'
    {131077, V1} -> 
        {cut, 'dec_Cut'(V1, [])};

      Else -> 
         exit({error,{asn1,{invalid_choice_tag,Else}}})
   end
.

%%%
%%% Run-time functions.
%%%

'dialyzer-suppressions'(Arg) ->
    ok.

ber_decode_nif(B) ->
    asn1rt_nif:decode_ber_tlv(B).

collect_parts(TlvList) ->
    collect_parts(TlvList, []).

collect_parts([{_,L}|Rest], Acc) when is_list(L) ->
    collect_parts(Rest, [collect_parts(L)|Acc]);
collect_parts([{3,<<Unused,Bits/binary>>}|Rest], _Acc) ->
    collect_parts_bit(Rest, [Bits], Unused);
collect_parts([{_T,V}|Rest], Acc) ->
    collect_parts(Rest, [V|Acc]);
collect_parts([], Acc) ->
    list_to_binary(lists:reverse(Acc)).

collect_parts_bit([{3,<<Unused,Bits/binary>>}|Rest], Acc, Uacc) ->
    collect_parts_bit(Rest, [Bits|Acc], Unused + Uacc);
collect_parts_bit([], Acc, Uacc) ->
    list_to_binary([Uacc|lists:reverse(Acc)]).

decode_octet_string(Tlv, TagsIn) ->
    Bin = match_and_collect(Tlv, TagsIn),
    binary:copy(Bin).

encode_length(L) when L =< 127 ->
    {[L],1};
encode_length(L) ->
    Oct = minimum_octets(L),
    Len = length(Oct),
    if
        Len =< 126 ->
            {[128 bor Len|Oct],Len + 1};
        true ->
            exit({error,{asn1,too_long_length_oct,Len}})
    end.

encode_restricted_string(OctetList, TagIn) when is_binary(OctetList) ->
    encode_tags(TagIn, OctetList, byte_size(OctetList));
encode_restricted_string(OctetList, TagIn) when is_list(OctetList) ->
    encode_tags(TagIn, OctetList, length(OctetList)).

encode_tags([Tag|Trest], BytesSoFar, LenSoFar) ->
    {Bytes2,L2} = encode_length(LenSoFar),
    encode_tags(Trest,
                [Tag,Bytes2|BytesSoFar],
                LenSoFar + byte_size(Tag) + L2);
encode_tags([], BytesSoFar, LenSoFar) ->
    {BytesSoFar,LenSoFar}.

match_and_collect(Tlv, TagsIn) ->
    Val = match_tags(Tlv, TagsIn),
    case Val of
        [_|_] = PartList ->
            collect_parts(PartList);
        Bin when is_binary(Bin) ->
            Bin
    end.

match_tags({T,V}, [T]) ->
    V;
match_tags({T,V}, [T|Tt]) ->
    match_tags(V, Tt);
match_tags([{T,V}], [T|Tt]) ->
    match_tags(V, Tt);
match_tags([{T,_V}|_] = Vlist, [T]) ->
    Vlist;
match_tags(Tlv, []) ->
    Tlv;
match_tags({Tag,_V} = Tlv, [T|_Tt]) ->
    exit({error,{asn1,{wrong_tag,{{expected,T},{got,Tag,Tlv}}}}}).

minimum_octets(0, Acc) ->
    Acc;
minimum_octets(Val, Acc) ->
    minimum_octets(Val bsr 8, [Val band 255|Acc]).

minimum_octets(Val) ->
    minimum_octets(Val, []).
