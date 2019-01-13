-module(ws_misc).
-import(typecast, [b2i/1]).
-export([
	version/1,
	format_ip/1
]).

version(_St) -> read_file("version").

read_file(Name) ->
	{ok, Binary} = file:read_file(Name),
	#{ version => Binary }.

format_ip({{A,B,C,D},Port}) -> lists:flatten(io_lib:format("~p.~p.~p.~p:~p", [A,B,C,D,Port])).
