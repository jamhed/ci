-module(ws_repo).
-include_lib("include/ws_state.hrl").
-export([get/1]).

get(S=#ws_state{user_id=UserId}) ->
	Repos = db_repo:getByUser(UserId),
	{reply, db_records:to_map(Repos), S}.
