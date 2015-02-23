-module(smp_js).
-compile({parse_transform, shen}).
-compile(export_all).
-jsmacro([auth/3]).

auth(Username,Token,Services) ->
    ws:send(enc(tuple(atom("Auth"),bin(Username),bin(Token),Services))).
