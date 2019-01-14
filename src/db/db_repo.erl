-module(db_repo).
-include("include/repo.hrl").
-include_lib("stdlib/include/qlc.hrl").
-compile({no_auto_import,[get/1]}).

-export([
	create/3, get/0, get/1, delete/1, update/2
]).

re({atomic, [Re]}) -> Re;
re(_) -> none.

create(Id, Name, Url) ->
	case get(Id) of
		#repo{} = Rec ->
			{atomic, ok} = mnesia:transaction(fun() -> mnesia:write(Rec#repo{name=Name, url=Url}) end);
		_ -> write(Id, Name, Url)
	end.

write(Id, Name, Url) ->
	Rec = #repo{id=Id, name=Name, url=Url},
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
