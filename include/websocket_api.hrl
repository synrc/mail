-ifndef(_WEBSOCKET_API_HRL_).
-define(_WEBSOCKET_API_HRL_, true).

-record('User',{id, firstName, lastName, login, status, photo}).
-record('Message',{room, sender, text, media, createdAt, starred}).
-record('Room',{users, id, lastMessage, unreadMessagesCount}).
-record('Authenticate',{token}).
-record('GetUserInfo',{}).
-record('GetRoomList',{}).
-record('GetRecentMessages',{room}).
-record('GetStarredMessages',{beforeDate, count}).
-record('GetMessages',{room, date, direction, count}).
-record('SendMessage',{room, message}).
-record('FindMessages',{room, starred, query}).
-record('Typing',{room, user}).
-record('StarMessages',{room, dates}).
-record('DeleteMessages',{room, dates}).
-record('ClearRoomHistory',{room}).
-record('MarkRoomAsRead',{room}).
-record('StatusChanged',{user, status}).
-record('CreateRoomResponse',{room, users}).
-record('RoomListChanged',{}).
-record('MessageReceived',{room, message}).

-endif.

