-ifndef(PROFILE_HRL).
-define(PROFILE_HRL, true).

-type profileStatus() :: remove_profile | get_profile | patch_profile.

-record('Profile',      {phone    = [] :: [] | binary(),
                         services = [] :: list(#'Service'{}),
                         rosters  = [] :: list(integer()),
                         settings = [] :: list(#'Feature'{}),
                         update   = 0  :: integer(),
                         balance  = 0  :: integer(),
                         presence = offline :: presence(),
                         profileStatus   = [] :: profileStatus() }).


-endif.
