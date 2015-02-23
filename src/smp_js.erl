-module(smp_js).
-compile({parse_transform, shen}).
-compile(export_all).
-jsmacro([auth/3]).

>  iolist_to_binary(smp_js:auth("'maxim'","'123'","'12'")).
<<"ws.send(enc(tuple(atom('Auth'),bin('maxim'),bin('123'),'12')));\n">>

auth(Username,Token,Services) ->
    ws:send(enc(tuple(atom("Auth"),bin(Username),bin(Token),Services))).
