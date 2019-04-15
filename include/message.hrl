-ifndef(MESSAGE_HRL).
-define(MESSAGE_HRL, true).
-include("feature.hrl").

-type container()     :: chain | cur.
-type messageType()   :: sys | reply | forward | read | edited | cursor.
-type messageStatus() :: masync | mdelete | mclear| mupdate | medit.
-type historyType()   :: updated | get | update | last_loaded | last_msg | get_reply.

-record(muc,            {name = [] :: [] | binary() }).
-record(p2p,            {from = [] :: [] | binary(),
                         to   = [] :: [] | binary() }).
-record('Ack',          {id    = [] :: [] | integer(),
                         table = [] :: [] | atom() }).

-record('Message',      {id        = [] :: [] | integer(),
                         client_id = [] :: [] | binary(),
                         from      = [] :: [] | binary(),
                         to        = [] :: [] | binary(),
                         files     = [] :: list(#'File'{}),
                         type      = [] :: messageType(),
                         link      = [] :: [] | integer() | #'Message'{},
                         mgs_status= [] :: messageStatus()}).

-record('History',      {feed      = [] :: [] | #p2p{} | #muc{},
                         size      = 0  :: [] | integer(),
                         entity_id = 0  :: [] | integer(),
                         data      = [] :: integer(),
                         status    = [] :: historyType()}).


-endif.
