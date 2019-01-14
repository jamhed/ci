-module(db_repo).
-include("include/repo.hrl").
-include_lib("stdlib/include/qlc.hrl").
-compile({no_auto_import,[get/1]}).

-export([
	get/0, create/0, get/1, delete/1, update/2
]).

re({atomic, [Re]}) -> Re;
re(_) -> none.

create() ->
	Rec = #repo{},
	{atomic, ok} = mnesia:transaction(fun() -> mnesia:write(Rec) end),
	Rec.

get() ->
	Q = qlc:q([ A || A=#repo{} <- mnesia:table(repo) ]),
	{atomic, Re} = mnesia:transaction(fun() -> qlc:e(Q) end),
	Re.

get(Id) ->
	re(mnesia:transaction(fun() -> mnesia:read({repo, Id}) end)).

delete(Id) ->
	mnesia:transaction(fun() -> db_links:delete(repo, Id) end).

update(Id, M) ->
	Rec = #repo{} = (db_records:from_map(repo, M))#repo{id=Id},
	{atomic, ok} = mnesia:transaction(fun() -> mnesia:write(Rec) end),
	Rec.
