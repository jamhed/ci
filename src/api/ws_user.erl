-module(ws_user).
-include_lib("include/ws_state.hrl").
-import(typecast, [l2b/1]).
-export([login/3, logout/1, websocket_info/2]).

% stub, allow to login master with any password
login(<<"master">>=L, _Password, S=#ws_state{peer=Peer}) ->
	UserId = master,
	gproc:reg({n, l, {ws, UserId}}, l2b(Peer)),
	self() ! {auth, #{ name => UserId }},
	{reply, #{ login => L, name => L }, S#ws_state{user_id=master}}.

logout(#ws_state{}) -> self() ! logout.

websocket_info({auth, User}, _S) -> #{ event => 'user-auth', user => User };
websocket_info(logout, _S) -> {stop, #{ event => 'user-logout' }}.
