-module(roster).
-behaviour(application).
-behaviour(supervisor).
-include_lib("n2o/include/n2o.hrl").
-include_lib("kvx/include/metainfo.hrl").
-compile(export_all).

stop(_)    -> ok.
start()    -> start(normal,[]).
start(_,_) -> X = supervisor:start_link({local,roster},roster,[]),
              syn:init(),
              kvx:join(),
              cowboy:start_tls(http, [{port, port()},
                     {certfile, code:priv_dir(roster)++"/ssl/fullchain.pem"},
                     {keyfile, code:priv_dir(roster)++"/ssl/privkey.pem"},
                     {cacertfile, code:priv_dir(roster)++"/ssl/fullchain.pem"}],
                      #{ env => #{dispatch => points()} }),
              [ n2o_pi:start(#pi{module=roster_vnode,table=ring,sup=roster,state=[],name={server,Pos}})
                || {{_,_},Pos} <- lists:zip(n2o:ring(),lists:seq(1,length(n2o:ring()))) ],
              X.
points()   -> cowboy_router:compile([{'_', [{"/[...]",n2o_cowboy2,[] } ]}]).
port()     -> application:get_env(n2o,port,8042).
init([])   -> {ok, {{one_for_one, 5, 10}, [ ] }}.
metainfo() -> #schema { name=roster, tables=[]}.
