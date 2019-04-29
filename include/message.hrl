-ifndef(MESSAGE_HRL).
-define(MESSAGE_HRL, true).

-record('MUC', { name      = [] :: [] | binary() }).
-record('P2P', { from      = [] :: [] | binary(),
                 to        = [] :: [] | binary() }).
-record('Ack', { id        = [] :: [] | integer(),
                 table     = [] :: [] | atom() }).
-record('Bin', { id        = [] :: binary(),
                 mime      = <<"text">> :: binary(),
                 payload   = [] :: binary()}).
-record('Msg', { id        = [] :: [] | integer(),
                 feed      = [] :: [] | #'MUC'{} | #'P2P'{},
                 files     = [] :: list(#'Bin'{}),
                 type      = [] :: [] | atom()}).
-record('Log', { feed      = [] :: [] | #'MUC'{} | #'P2P'{},
                 id        = [] :: [] | integer(),
                 data      = [] :: list(#'Msg'{}),
                 length    = [] :: [] | integer()}).

-endif.
