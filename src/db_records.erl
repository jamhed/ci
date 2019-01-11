-module(db_records).
-include("include/user.hrl").
-import(typecast, [b2ea/1, b2float/1, b2int/1, b2l/1, l2b/1]).

-export([fields/1,
	from_map/1, from_map/2, as_map/1, to_map/1, enrich_map/1, enrich_map/2
]).

fields(user) -> record_info(fields, user).

from_map(L) when is_list(L) -> [ from_map(M) || M <- L ];
from_map(#{ <<"record">> := Record }=M) ->
	from_map(erlang:binary_to_atom(Record, utf8), M).

from_map(Rec, M) ->
	Ma = typecat:keys_to_atoms(M),
	L = lists:foldl(
		fun(Field, List) ->
			[ from_map_value(Rec, Field, maps:get(Field, Ma)) | List]
		end,
		[Rec],
		fields(Rec)
	),
	erlang:list_to_tuple(lists:reverse(L)).

from_map_value(_, _, V) -> V.

to_map_value(_, K, V) -> {K, V}.

enrich_map(M, Without) when is_map(M), is_list(Without) -> enrich_map(maps:without(Without, M));
enrich_map(_M, _L) ->
	erlang:error(enrich_map_arguments),
	#{}.

enrich_map(L) when is_list(L) -> [ enrich_map(M) || M <- L ];
enrich_map(Map) when is_map(Map) ->
	MapName = maps:get(record, Map),
	lists:foldl(
		fun({K, V}, M) -> alter_map(M, to_map_value(MapName, K, V)) end,
		Map,
		maps:to_list(Map)
	);
enrich_map(_Crap) ->
	lager:warning("enrich_map:~p", [_Crap]),
	#{}.

to_map(L) when is_list(L) -> [ to_map(E) || E <- L ];
to_map(Rec) when is_tuple(Rec) ->
	[MapName|Values] = erlang:tuple_to_list(Rec),
	M = lists:foldl(
		fun({K,V}, M) -> alter_map(M, to_map_value(MapName, K, V)) end,
		#{},
		lists:zip(fields(MapName), Values)
	),
	M#{ record => MapName }.

as_map(Rec) when is_tuple(Rec) ->
	[MapName|Values] = erlang:tuple_to_list(Rec),
	M = lists:foldl(
		fun({K,V}, M) -> alter_map(M, {K, V}) end,
		#{},
		lists:zip(fields(MapName), Values)
	),
	M#{ record => MapName }.

alter_map(M, {K, V}) -> M#{K => V};
alter_map(M, []) -> M;
alter_map(M, [{K, V} | KV]) -> alter_map(M#{K => V}, KV).

