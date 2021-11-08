-module(chat).
-text('CHAT Application').
-behaviour(application).
-behaviour(supervisor).
-include("ROSTER.hrl").
-include_lib("n2o/include/n2o.hrl").
-include_lib("kvs/include/metainfo.hrl").
-compile(export_all).

stop(_)    -> ok.
start()    -> start(normal,[]).
port()     -> application:get_env(n2o,port,8042).
init([])   -> {ok, {{one_for_one, 5, 10}, [ ] }}.
metainfo() -> #schema { name=roster, tables=[#table{name='Pub', fields=record_info(fields,'Pub')}]}.
points()   -> cowboy_router:compile([{'_', [{ "/ws/[...]", n2o_cowboy, []}]}]).
start(_,_) -> kvs:join(),
              cowboy:start_clear(http,[{port,port()}],#{env=>#{dispatch=>points()}}),
              X = supervisor:start_link({local,?MODULE},?MODULE,[]),
              n2o:start_ws(),
              X.

fmt()      -> application:get_env(n2o,formatter,chat_ber).
bin(Key)   -> list_to_binary(io_lib:format("~p",[Key])).
user(Id)   -> case kvs:get(writer,"/chat/"++Id) of {ok,_} -> true; {error,_} -> false end.
format_msg(#'Pub'{key=Id,adr=#'Adr'{src=From,dst={p2p,#'P2P'{dst=To}}},bin=P}) ->
    io_lib:format("~s:~s:~s:~s",[From,To,Id,P]).
