-ifndef(AUTH_HRL).
-define(AUTH_HRL, true).

-type authOs()        :: ios | android | web.
-type authType()      :: google_auth | facebook_auth | mobile_auth | email_auth |
                         voice | resend | verify | push | logout | get | delete | clear.
-type authStatus()    :: invalid_version | mismatch_user_data | number_not_allowed |
                         session_not_found | attempts_expired | invalid_sms_code |
                         invalid_jwt_code | permission_denied | invalid_data.

-record('Auth',         {client_id   = [] :: [] | binary(),
                         dev_key     = [] :: [] | binary(),
                         user_id     = [] :: [] | binary(),
                         token       = [] :: [] | binary(),
                         data        = [] :: [] | binary(),
                         type        = [] :: authType(),
                         attempts    = [] :: [] | integer(),
                         settings    = [] :: list(#'Feature'{}),
                         services    = [] :: list(#'Service'{}),
                         push        = [] :: [] | binary(),
                         os          = ios :: authOs(),
                         created     = [] :: [] | integer(),
                         last_online = [] :: [] | integer() }).

-record('AuthError',    {codes    = [] :: list(authStatus()),
                         data     = [] :: [] | #'Auth'{}}).


-endif.
