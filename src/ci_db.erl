-module(ci_db).
-import(typecast, [fmt/2]).
-include_lib("stdlib/include/qlc.hrl").
-include_lib("include/user.hrl").

-export([
	setup/0, create_tables/0, setup_mnesia/0,
	export_as_json/0, import_from_json/1, export/0, import/0, tables/0, create_tables/1
]).

tables() ->
	[
		{user, [], record_info(fields, user)}
	].

create_tables() -> create_tables(tables()).
create_tables([]) -> mnesia:wait_for_tables([ Table || {Table, _, _} <- tables()], 5000);
create_tables([ {Table, Index, Fields} | Tables ]) ->
	mnesia:create_table(Table, [ {disc_copies, [node()]}, {index, Index}, {attributes, Fields} ]),
	create_tables(Tables).

setup() ->
	lager:notice("initialize mnesia"),
	setup_mnesia(),
	create_tables().

setup_mnesia() ->
	mnesia:change_table_copy_type(schema, node(), disc_copies).

export_as_json() ->
	Modules = [ erlang:list_to_atom(fmt("db_~s", [Table])) || {Table, _, _} <- tables() ],
	Data = lists:flatten([ Module:get() || Module <- Modules ]),
	jiffy:encode(ws_main:replace_undefined(db_records:to_map(Data)), [pretty]).

import_from_json(JSON) ->
	Data = jiffy:decode(JSON, [return_maps]),
	Records = db_records:from_map(Data),
	[ db_misc:put(Record) || Record <- Records ].

export() ->
	file:write_file("ci_db.json", export_as_json()).
import() ->
	try
		{ok, Bin} = file:read_file("ci_db.json"),
		import_from_json(Bin)
	catch
		C:E ->
			lager:notice("error reading config database, initialize with defaults ~p:~p", [C, E]),
			no_file
	end.
