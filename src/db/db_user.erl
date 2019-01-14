-module(db_user).
-include("include/user.hrl").
-include_lib("stdlib/include/qlc.hrl").
-compile({no_auto_import,[get/1]}).

-export([
	get/0, create/2, get/1, delete/1, update/2
]).

re({atomic, [Re]}) -> Re;
re(_) -> none.

create(Id, Login) ->
	Rec = #user{id=Id, login=Login, ts=erlang:timestamp()},
	{atomic, ok} = mnesia:transaction(fun() -> mnesia:write(Rec) end),
	Rec.

get() ->
	Q = qlc:q([ A || A=#user{} <- mnesia:table(user) ]),
	{atomic, Re} = mnesia:transaction(fun() -> qlc:e(Q) end),
	Re.

get(Id) ->
	re(mnesia:transaction(fun() -> mnesia:read({user, Id}) end)).

delete(Id) ->
	mnesia:transaction(fun() -> db_links:delete(user, Id) end).

update(Id, M) ->
	Rec = #user{} = (db_records:from_map(user, M))#user{id=Id},
	{atomic, ok} = mnesia:transaction(fun() -> mnesia:write(Rec) end),
	Rec.
