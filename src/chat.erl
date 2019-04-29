-module(chat).
-behaviour(application).
-behaviour(supervisor).
-include("message.hrl").
-include_lib("n2o/include/n2o.hrl").
-include_lib("kvx/include/metainfo.hrl").
-compile(export_all).

stop(_)    -> ok.
start()    -> start(normal,[]).
port()     -> application:get_env(n2o,port,8042).
init([])   -> {ok, {{one_for_one, 5, 10}, [ ] }}.
metainfo() -> #schema { name=roster, tables=[#table{name='Msg', fields=record_info(fields,'Msg')}]}.
start(_,_) -> X = supervisor:start_link({local,?MODULE},?MODULE,[]),
              syn:init(),
              kvx:join(),
              cowboy:start_tls(http,n2o_cowboy:env(?MODULE),
                     #{env=>#{dispatch=>n2o_static:endpoints(?MODULE,n2o_static)}}),
              [ n2o_pi:start(#pi{module=n2o_wsnode,table=ring,
                                 sup=?MODULE,state=[],name={server,Pos}})
                || {{_,_},Pos} <- lists:zip(n2o:ring(),lists:seq(1,length(n2o:ring()))) ],
              X.
