-module(roster_chat).
-compile(export_all).
-include_lib("nitro/include/nitro.hrl").
-include_lib("kvs/include/user.hrl").
-include_lib("n2o/include/wf.hrl").

main()    -> #dtl{app=roster,bindings=[{body,body()}]}.

peer()    -> io_lib:format("~p",[wf:peer(?REQ)]).
message() -> wf:js_escape(wf:html_encode(wf:ql(message))).
body()    -> [ #label{body=["Logged as: ",wf:to_list((wf:user())#user.id)]}, #br{},
               #label{body=["Chat With: ",wf:to_list(select())]},
               #panel{id=history}, #textbox{id=message},
               #button{id=send,body="Chat",postback=chat,source=[message]} ].
select()  -> case {wf:q(user),wf:q(room)} of
                  {undefined,Room} -> wf:to_list(Room);
                  {Chat,undefined} -> wf:to_list(Chat);
                  {A,B} -> {error,"Wrong Parameters"} end.

event(init) -> ok;
event(chat) -> wf:insert_bottom(history,#panel{id=history,body=[peer(),": ",message(),#br{}]});
event(E) -> roster_api:event(E).
