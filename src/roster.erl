-module(roster).
-copyright('Synrc Research Center s.r.o.').
-author('Maxim Sokhatsky').
-include_lib("kvs/include/metainfo.hrl").
-include_lib("kvs/include/kvs.hrl").
-include_lib("kvs/include/user.hrl").
-include_lib("kvs/include/group.hrl").
-compile(export_all).

metainfo() -> #schema { name=roster,    tables=[]}.
log_modules() -> [  ].

start_link(Parameters) -> gen_server:start_link(?MODULE, Parameters, []).
init([]) -> RestartStrategy = one_for_one,
            MaxRestarts = 1,
            MaxSecondsBetweenRestarts = 600,
            SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
            {ok, {SupFlags, []}}.
