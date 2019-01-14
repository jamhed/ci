-module(ws_user).
-include_lib("include/ws_state.hrl").
-import(typecast, [l2b/1]).
-export([login/3, logout/1, profile/1, session/2, build/1, websocket_info/2]).

login(<<"master">>, _Password, S=#ws_state{}) ->
	UserId = master,
	SessionId = ws_session:register(NewS=S#ws_state{user_id=UserId}),
	self() ! {auth, #{ name => UserId }},
	{reply, SessionId, NewS};

login(<<"admin">>, _Password, S=#ws_state{}) ->
	UserId = admin,
	SessionId = ws_session:register(NewS=S#ws_state{user_id=UserId}),
	self() ! {auth, #{ name => UserId, role => admin }},
	{reply, SessionId, NewS}.

logout(#ws_state{}) -> self() ! logout.

profile(#ws_state{user_id=UserId}) ->
	#{ name => UserId }.

session(SessionId, #ws_state{}) ->
	ws_session:get(SessionId).

build(#ws_state{}) ->
	ok.

websocket_info({auth, User}, _S) -> #{ event => 'user-auth', user => User };
websocket_info(logout, _S) -> {stop, #{ event => 'user-logout' }}.
