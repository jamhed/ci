-module(db_misc).
-include_lib("stdlib/include/qlc.hrl").
-import(typecast, [l2b/1, b2l/1]).

-export([
	new_id/0, next_id/1, put/1, all/1
]).

new_id() ->
	erlang:list_to_binary(uuid:uuid_to_string(uuid:get_v4())).

next_id(Record) ->
	mnesia:dirty_update_counter({id_seq, Record}, 1).

put(Data) ->
	{atomic, ok} = mnesia:transaction(fun() -> mnesia:write(Data) end),
	Data.

all(Table) ->
	Q = qlc:q([ S || S <- mnesia:table(Table)]),
	{atomic, Records} = mnesia:transaction(fun() -> qlc:e(Q) end),
	Records.
