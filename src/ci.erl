-module(ci).
-include_lib("stdlib/include/qlc.hrl").

% utility functions to use in console

-export([
	regp/0, regn/0, reg/1, trace/0,
	dump_map/1
]).

regp() -> reg({l, p}).
regn() -> reg({l, n}).

reg(S) ->
	Q = qlc:q([ Prop || Prop <- gproc:table(S) ]),
	qlc:e(Q).

trace() ->
	{current_stacktrace, Stack} = process_info(self(), current_stacktrace),
	lager:error("stacktrace:~s", [lager:pr_stacktrace(Stack)]).

dump_map(M) ->
	Sorted = lists:sort(fun({A,_},{B,_}) -> B >= A end, maps:to_list(M)),
	[ lager:info("map ~s: ~p~n", [K,V]) || {K,V} <- Sorted ].
