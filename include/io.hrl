-ifndef(IO_HRL).
-define(IO_HRL, true).

-record(error, { err_id   = [] :: [] | binary() }).
-record(ok,    { ok_id    = [] :: [] | binary() }).

-record(rec,   { rec_id   = [] :: atom(),
                 rec_data = [] :: binary() | integer()   }).

-record(io,    { io_id    = [] :: [] | #ok{} | #error{},
                 io_data  = [] :: [] | <<>>  | #rec{}    }).

-endif.
