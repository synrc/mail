-ifndef(ROOM_HRL).
-define(ROOM_HRL, true).

-type roomType()      :: group | channel.
-type linkStatus()    :: lgen | lcheck | ladd | ldelete | lupdate.
-type memberStatus()  :: admin | member | removed | patch | owner.
-type container()     :: chain | cur.
-type roomStatus()    :: room_create | room_leave| room_add | room_remove | room_patch |
                         room_get | room_delete | room_last_msg.

-record(muc,            {name = [] :: [] | binary() }).
-record(p2p,            {from = [] :: [] | binary(),
                         to   = [] :: [] | binary() }).

-record('Link',         {id        = [] :: [] | binary(),
                         name      = [] :: [] | binary(),
                         room_id   = [] :: [] | binary(),
                         created   = 0  :: [] | integer(),
                         links_status    = [] :: linkStatus()}).

-record('Member',       {id        = [] :: [] | integer(),
                         container = chain :: container(),
                         feed_id   = [] :: #muc{} | #p2p{},
                         prev      = [] :: [] | integer(),
                         next      = [] :: [] | integer(),
                         feeds     = [] :: list(),
                         phone_id  = [] :: [] | binary(),
                         avatar    = [] :: [] | binary(),
                         names     = [] :: [] | binary(),
                         surnames  = [] :: [] | binary(),
                         alias     = [] :: [] | binary(),
                         reader    = 0  :: [] | integer(),
                         update    = 0  :: [] | integer(),
                         settings  = [] :: list(#'Feature'{}),
                         services  = [] :: list(#'Service'{}),
                         presence  = offline :: presence(),
                         member_status    = member :: memberStatus()}).

-record('Room',         {id          = [] :: [] | binary(),
                         name        = [] :: [] | binary(),
                         links       = [] :: [] | list(#'Link'{}),
                         description = [] :: [] | binary(),
                         settings    = [] :: list(#'Feature'{}),
                         members     = [] :: list(#'Member'{}),
                         admins      = [] :: list(#'Member'{}),
                         data        = [] :: list(#'Desc'{}),
                         type        = [] :: roomType(),
                         tos         = [] :: [] | binary(),
                         tos_update  = 0  :: [] | integer(),
                         unread      = 0  :: [] | integer(),
                         mentions    = [] :: list(integer()),
                         readers     = [] :: list(integer()),
                         last_msg    = [] :: [] | integer(), % #'Message'{}
                         update      = 0  :: [] | integer(),
                         created     = 0  :: [] | integer(),
                         status      = [] :: roomStatus()}).

-endif.
