-module(roster_user).
-author('Maxim Sokhatsky').
-include("SMP.hrl").
-include_lib("kvs/include/user.hrl").
-behaviour(gen_server).
-export(?GEN_SERVER).
-compile(export_all).

start_link(Parameters) -> gen_server:start_link(?MODULE, Parameters, []).

handle_call({get},_,Proc)              -> { reply,Proc,Proc };
handle_call(Command,_,Proc)            -> { reply,{unknown,Command},Proc }.

init(User) ->
    kvs:info(?MODULE,"User ~p spawned ~p",[User#user.id,self()]),
    U = case kvs:get(user,User#user.id) of
         {ok,Exists} -> Exists;
           {error,_} -> User end,
    wf:cache({user,U#user.id},self()),
    {ok, U#user{}}.

handle_cast(Msg, State) ->
    kvs:info(?MODULE,"Unknown API async: ~p", [Msg]),
    {stop, {error, {unknown_cast, Msg}}, State}.

handle_info({'DOWN', _MonitorRef, _Type, _Object, _Info} = Msg, State = #user{id=Id}) ->
    kvs:info(?MODULE, "connection closed, shutting down session:~p", [Msg]),
    wf:cache({user,Id},undefined),
    {stop, normal, State};

handle_info(Info, State=#user{}) ->
    kvs:info(?MODULE,"Unrecognized info: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, #user{id=Id}) ->
    kvs:info(?MODULE,"Terminating session Id cache: ~p", [Id]),
    wf:cache({user,Id},undefined),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

