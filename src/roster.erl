-module(roster).
-copyright('Synrc Research Center s.r.o.').
-author('Maxim Sokhatsky').
-include_lib("kvs/include/user.hrl").
-include("SMP.hrl").
-compile(export_all).

% user creation

create(User,Name,Surname) ->
    kvs:add(#user{id=kvs:next_id(user,1),
                  feed_id={users},
                  names=Name,
                  surnames=Surname}).

% chain users to other users (add/remove to/from roster)

add(User,#'Person'{}=Person) ->
    kvs:add(Person#'Person'{id=kvs:next_id('Person',1),
                            feed_id={User#user.id,roster}}).

remove(UserId, PersonId) ->
    kvs:remove(#'Person'{feed_id={UserId,roster},
                         id=PersonId}).

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
    kvs:entries(kvs:get(feed,{RoomId,chat}),'Public').

retrieve(UserId,UserWith,Count) ->
    kvs:entries(kvs:get(feed,{UserId,chat,UserWith}),'Private').
