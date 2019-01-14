-module(db_commit).
-include("include/commit.hrl").
-include_lib("stdlib/include/qlc.hrl").
-compile({no_auto_import,[get/1]}).

-export([
	get/0, getByRepo/1, create/3, delete/1
]).

re({atomic, [Re]}) -> Re;
re(_) -> none.

create(RepoId, Sha, Branch) ->
	case get(Sha) of
		#commit{} = Rec ->
			{atomic, ok} = mnesia:transaction(fun() -> mnesia:write(Rec#commit{branch=Branch, ts=erlang:timestamp()}) end);
		_ -> write(RepoId, Sha, Branch)
	end.

write(RepoId, Sha, Branch) ->
	Rec = #commit{sha=Sha, repo_id=RepoId, branch=Branch, ts=erlang:timestamp()},
	{atomic, ok} = mnesia:transaction(fun() -> mnesia:write(Rec) end),
	Rec.

get() ->
	Q = qlc:q([ A || A=#commit{} <- mnesia:table(commit) ]),
	{atomic, Re} = mnesia:transaction(fun() -> qlc:e(Q) end),
	Re.

get(Sha) ->
	re(mnesia:transaction(fun() -> mnesia:read({commit, Sha}) end)).

getByRepo(RepoId) ->
	Q = qlc:q([ A || A=#commit{repo_id=I} <- mnesia:table(commit), I =:= RepoId ]),
	{atomic, Re} = mnesia:transaction(fun() -> qlc:e(Q) end),
	Re.

delete(Sha) ->
	mnesia:transaction(fun() -> db_links:delete(commit, Sha) end).
