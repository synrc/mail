-module(roster_auth).
-author('Maxim Sokhatsky').
-compile(export_all).
-include_lib("n2o/include/wf.hrl").

source()               -> source(?CTX).
source(CTX=#cx{})      -> source(wf:qc(<<"from">>,CTX));
source(<<"website">>)  -> website;
source(<<"mobile">>)   -> mobile;
source(<<"dev">>)      -> dev;
source(_)              -> cheater.

auth() -> Source      = source(),
          SourceSID   = wf:cookie_req(n2o_session:session_cookie_name(Source), ?REQ),
          auth(Source, SourceSID).

auth(Source,SourceSID) ->
    wf:info(?MODULE,"AUTH: Client will be authenticated later.~n",[]),
    wf:session(<<"session">>, {session,Source,SourceSID}).
