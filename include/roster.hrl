-ifndef(ROSTER_HRL).
-define(ROSTER_HRL, true).

-type rosterStatus()  :: get_roster | create_roster | del_roster | remove_roster| nick| add_roster | update_roster |
                         list_loster | patch_roster | roster_last_msg.
-type contactStatus() :: conact_request | authorization | contact_ignore | conatct_internal |
                         friend | contact_last_msg | contact_ban | conact_banned | contact_deleted.

-record(muc,            {name = [] :: [] | binary() }).
-record(p2p,            {from = [] :: [] | binary(),
                         to   = [] :: [] | binary() }).

-record('Contact',      {user_id = [] :: [] | binary(),
                         avatar   = [] :: list(integer()), % #'Desc'{} from message
                         names    = [] :: [] | binary(),
                         surnames = [] :: [] | binary(),
                         nick     = [] :: [] | binary(),
                         reader   = [] :: [] | list(integer()),
                         unread   = 0  :: [] | integer(),
                         last_msg = [] :: [] | integer(),
                         update   = 0  :: [] | integer(),
                         created  = 0  :: [] | integer(),
                         settings = [] :: list(#'Feature'{}),
                         services = [] :: list(#'Service'{}),
             		         presence = offline :: presence(),
                         status   = [] :: contactStatus()}).

-record('Roster',       {id       = [] :: [] | integer(),
                         names    = [] :: [] | binary(),
                         surnames = [] :: [] | binary(),
                         email    = [] :: [] | binary(),
                         nick     = [] :: [] | binary(),
                         userlist = [] :: list(#'Contact'{}),
                         roomlist = [] :: list(integer()),
                         favorite = [] :: list(integer()),
                         tags     = [] :: list(integer()),
                         phone    = [] :: [] | binary(),
                         avatar   = [] :: [] | binary(),
                         update   = 0  :: [] | integer(),
                         rosterStatus   = [] :: rosterStatus() }).

-endif.
