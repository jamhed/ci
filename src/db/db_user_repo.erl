-module(db_user_repo).
-include("include/user.hrl").
-include_lib("stdlib/include/qlc.hrl").
-compile({no_auto_import,[get/1]}).

-export([
	get/0, getByUserId/1, create/3, delete/1
]).

create(UserId, RepoId, Access) ->
	Rec = #user_repo{user_id=UserId, repo_id=RepoId, access=Access},
	{atomic, ok} = mnesia:transaction(fun() -> mnesia:write(Rec) end),
	Rec.

get() ->
	Q = qlc:q([ A || A=#user_repo{} <- mnesia:table(user_repo) ]),
	{atomic, Re} = mnesia:transaction(fun() -> qlc:e(Q) end),
	Re.

getByUserId(UserId) ->
	Q = qlc:q([ A || A=#user_repo{user_id=I} <- mnesia:table(user_repo), I =:= UserId ]),
	{atomic, Re} = mnesia:transaction(fun() -> qlc:e(Q) end),
	Re.

delete(Id) ->
	mnesia:transaction(fun() -> db_links:delete(user_repo, Id) end).
