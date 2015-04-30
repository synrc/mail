-module(roster).
-copyright('Synrc Research Center s.r.o.').
-author('Maxim Sokhatsky').
-include_lib("kvs/include/metainfo.hrl").
-include_lib("kvs/include/kvs.hrl").
-include_lib("kvs/include/user.hrl").
-include_lib("kvs/include/group.hrl").
-include("SMP.hrl").
-compile(export_all).

% user/group creation

create_user(User,Name,Surname) ->
    {ok,Stored} = kvs:add(#user{id=kvs:next_id(user,1),names=Name,surnames=Surname}),
    roster_app:worker(user,Stored).

create_group(Name) ->
    {ok,Stored} = kvs:add(#group{id=kvs:next_id(group,1),name=Name}),
    roster_app:worker(group,Stored).

% chain users to other users (add/remove to/from roster)

add(#user{}=User,#'RosterItem'{}=Person) ->
    kvs:add(Person#'RosterItem'{id=kvs:next_id('RosterItem',1),feed_id={roster,User#user.id}}).

remove(UserId, RosterItemId) ->
    kvs:remove(#'RosterItem'{feed_id={roster,UserId},id=RosterItemId}).

list(UserId) ->
    kvs:entries(kvs:get(feed,{roster,UserId}),'RosterItem',-1).

join(UserId,RoomId) ->
    kvs:add(#'MessageItem'{feed_id={history,RoomId},
                      id=kvs:next_id('MessageItem',1),
                      payload=io_lib:format("User ~p joined.",[UserId]),
                      origin=system,
                      recipient={room,RoomId}}).

% chain messages to the chat lists

private(FromId,ToId,Private) ->
    kvs:add(Private#'MessageItem'{id=kvs:next_id('MessageItem',1),feed_id={chat,ToId}}).

public(FromId,ToId,Public) ->
    kvs:add(Public#'MessageItem'{id=kvs:next_id('MessageItem',1),feed_id={room,ToId}}).

% retrival of user device that should be stored in tokens during auth

token(User,DeviceId) ->
    lists:keyfind(DeviceId,1,User#user.tokens).

% retrival of last Count messages in chat history

retrieve(Feed,Count) ->
    kvs:entries(kvs:get(feed,Feed),'MessageItem',Count).

% roster KVS metainfo

metainfo() -> #schema { name=roster,    tables=[
              #table  { name='RosterItem',  fields=record_info(fields, 'RosterItem')},
              #table  { name='MessageItem', fields=record_info(fields, 'MessageItem')}]}.

start_link(Parameters) -> gen_server:start_link(?MODULE, Parameters, []).
init([]) -> RestartStrategy = one_for_one,
            MaxRestarts = 1,
            MaxSecondsBetweenRestarts = 600,
            SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
            {ok, {SupFlags, []}}.

log_modules() -> [ roster,
                   roster_app,
                   roster_user,
                   roster_group,
                   roster_api,
                   roster_auth,
                   roster_index,
                   roster_chat   ].

