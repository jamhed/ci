-module(db_links).
-include_lib("include/db.hrl").
-include_lib("stdlib/include/qlc.hrl").
-export([delete/1, delete/2, clear/3]).

-define(CLEAR(Table, Field),
clear(Table, Field, Id) ->
	Q = qlc:q([ A || A=#Table{Field=I} <- mnesia:table(Table), I =:= Id ]),
	[ mnesia:write(A#Table{Field=undefined}) || A <- qlc:e(Q) ]
).

-define(DELETE(Table, Field),
delete(Table, Field, Id) ->
	Q = qlc:q([ A || A=#Table{Field=I} <- mnesia:table(Table), I =:= Id ]),
	[ mnesia:delete({Table, ObjId}) || #Table{id=ObjId} <- qlc:e(Q) ]
).

?CLEAR(user, id).

% mnesia:delete compatibility
delete({Table, Id}) ->
	delete(Table, Id).

delete(Table, Id) ->
	mnesia:delete({Table, Id}).
