-module(roster).
-copyright('Synrc Research Center s.r.o.').
-author('Maxim Sokhatsky').
-include_lib("kvs/include/metainfo.hrl").
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

add(User,#'Person'{}=Person) ->
    kvs:add(Person#'Person'{id=kvs:next_id('Person',1),
                            feed_id={User#user.id,roster}}).

remove(UserId, PersonId) ->
    kvs:remove(#'Person'{feed_id={UserId,roster},
                         id=PersonId}).

list(UserId) ->
    kvs:entries(kvs:get(feed,{UserId,roster}),'Person',-1).

join(UserId,RoomId) ->
    kvs:add(#'Public'{feed_id={RoomId,chat},
                      id=kvs:next_id('Public',1),
                      message=io_lib:format("User ~p joined.",[UserId]),
                      author=system,
                      room=RoomId}).

% chain messages to the chat lists

private(FromId,ToId,Private) ->
    kvs:add(Private#'Private'{id=kvs:next_id('Private',1),
                              feed_id={FromId,chat,ToId}}).

public(FromId,ToId,Public) ->
    kvs:add(Public#'Public'{id=kvs:next_id('Public',1),
                            feed_id={ToId,chat}}).

% retrival of user device that should be stored in tokens during auth

token(User,DeviceId) ->
    lists:keyfind(DeviceId,1,User#user.tokens).

% retrival of last Count messages in chat history

retrieve(RoomId,Count) ->
    kvs:entries(kvs:get(feed,{RoomId,chat}),'Public',Count).

retrieve(UserId,UserWith,Count) ->
    kvs:entries(kvs:get(feed,{UserId,chat,UserWith}),'Private',Count).

% roster KVS metainfo

metainfo() -> #schema { name=roster,    tables=[
              #table  { name='Person',  fields=record_info(fields, 'Person')},
              #table  { name='Public',  fields=record_info(fields, 'Public')},
              #table  { name='Private', fields=record_info(fields, 'Private')}]}.

start_link(Parameters) -> gen_server:start_link(?MODULE, Parameters, []).
init([]) -> RestartStrategy = one_for_one,
            MaxRestarts = 1,
            MaxSecondsBetweenRestarts = 600,
            SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
            {ok, {SupFlags, []}}.

log_modules() -> [roster,roster_app,roster_user,roster_group].
