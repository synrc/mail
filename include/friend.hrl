-ifndef(FRIEND_HRL).
-define(FRIEND_HRL, true).

-type friendStatus() :: ban | unban | request | confirm | update | ignore.

-record('Friend',       {phone_id  = [] :: [] | binary(),
                         friend_id = [] :: [] | binary(),
                         settings  = [] :: list(#'Feature'{}),
                         status    = [] :: friendStatus()}).

-endif.
