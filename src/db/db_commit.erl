-module(db_commit).
-include("include/commit.hrl").
-include_lib("stdlib/include/qlc.hrl").
-compile({no_auto_import,[get/1]}).

-export([
	get/0, getByRepo/1, create/2, delete/1
]).

create(RepoId, Sha) ->
	Rec = #commit{sha=Sha, repo_id=RepoId, ts=erlang:timestamp()},
	{atomic, ok} = mnesia:transaction(fun() -> mnesia:write(Rec) end),
	Rec.

get() ->
	Q = qlc:q([ A || A=#commit{} <- mnesia:table(commit) ]),
	{atomic, Re} = mnesia:transaction(fun() -> qlc:e(Q) end),
	Re.

getByRepo(RepoId) ->
	Q = qlc:q([ A || A=#commit{repo_id=I} <- mnesia:table(commit), I =:= RepoId ]),
	{atomic, Re} = mnesia:transaction(fun() -> qlc:e(Q) end),
	Re.

delete(Sha) ->
	mnesia:transaction(fun() -> db_links:delete(commit, Sha) end).
