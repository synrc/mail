-module(roster_routes).
-author('Maxim Sokhatsky').
-include_lib("n2o/include/wf.hrl").
-export([init/2, finish/2]).

finish(State, Ctx) -> {ok, State, Ctx}.
init(State, Ctx) ->
    Path = wf:path(Ctx#cx.req),
    wf:info(?MODULE,"Route: ~p~n",[Path]),
    {Controller,Page} = route_prefix(Path),
    {ok, State, Ctx#cx{path=Controller,module=Page}}.

route_prefix(<<"/ws/",P/binary>>) -> route(P);
route_prefix(<<"/",P/binary>>) -> route(P);
route_prefix(P) -> route(P).

route(<<>>=A)              -> {A,roster_login};
route(<<"index">>=A)       -> {A,roster_index};
route(<<"api">>=A)         -> {A,roster_api};
route(<<"chat">>=A)        -> {A,roster_chat};
route(<<"login">>=A)       -> {A,roster_login};
route(<<"favicon.ico">>=A) -> {A,static_file};
route(A) ->                   {A,roster_login}.
