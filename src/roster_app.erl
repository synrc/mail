-module(roster_app).
-behaviour(application).
-include_lib("kvs/include/user.hrl").
-include_lib("kvs/include/group.hrl").
-include_lib("kvs/include/feed.hrl").
-compile(export_all).
-export([start/2, stop/1]).

chat(#user{id=Id}=User)   -> {Id,{roster_user,start_link,[User]},
                             permanent,2000,worker,[roster_user]}.

room(#group{id=Id}=Group) -> {Id,{roster_group,start_link,[Group]},
                             permanent,2000,worker,[roster_group]}.

pool(SupName)             -> {SupName,{supervisor,start_link,[{local,SupName},roster,[]]},
                             permanent,infinity,supervisor,[]}.

stop(_)    -> ok.
start()    -> start(normal,[]).
start(_,_) ->
    Sup = supervisor:start_link({local, roster_sup}, ?MODULE, []),

    [ case kvs:get(feed,Space) of
           {ok,Feed} -> kvs:fold(fun(A,Acc) -> worker(Space,A) end,[],Space,
                            Feed#feed.top,undefined,
                            #iterator.prev,#kvs{mod=store_mnesia});
                  __ -> skip end || Space <- [user,group] ],

    Sup.

init([]) ->
    application:set_env(kvs,dba,store_mnesia),
    kvs:join(),

     cowboy:start_http(http, 3, [{port, wf:config(n2o,port)}],
                                [{env, [{dispatch, dispatch_rules()}]}]),

    {ok, {{one_for_one, 5, 10}, [ pool(chat_sup),
                                  pool(muc_sup)
                                ] }}.

worker(user,User)   -> {ok, Pid} = supervisor:start_child(chat_sup,chat(User)), {ok, {Pid,User#user.id}};
worker(group,Group) -> {ok, Pid} = supervisor:start_child(muc_sup,room(Group)), {ok, {Pid,Group#group.id}}.

mime() -> [{mimetypes,cow_mimetypes,all}].

dispatch_rules() ->
    cowboy_router:compile(
        [{'_', [
            {"/static/[...]", n2o_dynalo, {dir, "priv/static", mime()}},
            {"/n2o/[...]", n2o_dynalo, {dir, "deps/n2o/priv", mime()}},
            {"/ws/[...]", bullet_handler, [{handler, n2o_bullet}]},
            {'_', n2o_cowboy, []}
    ]}]).
