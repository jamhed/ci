-module(ws_main).
-include_lib("include/ws_state.hrl").

-export([
	init/2, websocket_init/1, websocket_handle/2, websocket_info/2, terminate/3
]).

init(Req, _Opts) ->
	{cowboy_websocket, Req, [Req], #{compress => true}}.

websocket_init([#{} = Req]) ->
	lager:info("start websocket ~p", [Req]),
	{ok, #ws_state{request=Req, peer=get_peer(Req)}}.

websocket_handle({text, <<"ping">>}, S=#ws_state{peer=Peer}) ->
	lager:debug("~s js client ping", [Peer]),
	{ok, S};

websocket_handle({text, Msg}, S=#ws_state{peer=Peer}) ->
	{Re, NewS=#ws_state{user_id=UserId}} = handle_msg(decode(Msg), S),
	lager:debug("~s user_id:~w ws out ~p", [Peer, UserId, Re]),
	{reply, {text, encode(Re)}, NewS};

% pong is sent automatically by cowboy, just ignore
websocket_handle(ping, S=#ws_state{peer=Peer}) ->
	lager:debug("~s ws ping", [Peer]),
	{ok, S};

websocket_handle(_Data, S=#ws_state{peer=Peer, user_id=UserId}) ->
	lager:error("~s user_id:~w unhandled data:~p", [Peer, UserId, _Data]),
	{ok, S}.

websocket_info({'DOWN', _Ref, process, _Pid, _Reason}, S=#ws_state{peer=Peer, user_id=UserId}) when is_pid(_Pid) ->
	lager:notice("~s user_id:~w is dead pid:~p reason:~p, stop ws", [Peer, UserId, _Pid, _Reason]),
	{stop, S};

websocket_info(stop, S=#ws_state{peer=Peer, user_id=UserId}) ->
	lager:notice("~s user_id:~w logout", [Peer, UserId]),
	{stop, S};

websocket_info(Info, S=#ws_state{peer=Peer, user_id=UserId}) ->
	try
		ws_user:websocket_info(Info, S) of
			ok -> {ok, S};
			stop -> {stop, S};
			{stop, M} when is_map(M) ->
				erlang:send_after(100, self(), stop),
				{reply, {text, encode(M)}, S};
			M when is_map(M) ->
				lager:info("~s user_id:~w event ~p", [Peer, UserId, M]),
				{reply, {text, encode(M)}, S}
	catch
		C:E ->
			lager:error("error handle event: ~p~n~s", [Info, lager:pr_stacktrace(erlang:get_stacktrace(), {C, E})]),
			erlang:error(websocket_error), % crash deliberately to make errors visible to the ui
			{ok, S}
	end.

terminate(Reason, _Req, S) ->
	lager:debug("terminate, reason:~p, state:~p", [Reason, S]),
	ok.

handle_msg(#{ <<"type">> := <<"call">>, <<"args">> := [M, F, A], <<"id">> := Id }, S=#ws_state{}) when is_list(A) ->
	Reply = maybe_call(M, F, A, S),
	{Map, NewS} = encode_reply(Reply, S),
	{Map#{ id => Id }, NewS}.

encode_reply({error, Error, S}, _S) -> {#{ error => Error }, S};
encode_reply({error, Error}, S) -> {#{ error => Error }, S};
encode_reply({reply, Re, S}, _S) -> {#{ reply => Re}, S};
encode_reply(Re, S) -> {#{reply => Re}, S}.

maybe_call(M, F, A, S=#ws_state{peer=Peer, user_id=UserId}) when is_list(A) ->
	lager:info("~s user_id:~p api ~s:~s ~p", [Peer, UserId, M, F, typecast(A)]),
	try
		erlang:apply(b2a(M), b2a(F), A++[S])
	catch
		% known errors
		error:Err when is_atom(Err); is_binary(Err) ->
			lager:error("ws api ~s:~s ~p~s", [M, F, A, lager:pr_stacktrace(erlang:get_stacktrace(), {error, Err})]),
			{error, Err};
		error:{typecheck, Field, Err} -> {error, [Field, Err]};
		error:{error,{cant_resolve=Err, Address}} ->
			lager:warning("can't resolve address: ~p", [Address]),
			{error, Err};
		C:E ->
			% all errors must be known, die otherwise
			lager:error("~s user_id:~p, ~s args:~p ~s", [Peer, UserId, F, A, lager:pr_stacktrace(erlang:get_stacktrace(), {C, E})]),
			erlang:error(E)
	end.

decode(Msg) -> jiffy:decode(Msg, [return_maps]).

encode(M) ->
	try
		jiffy:encode(replace_undefined(M))
	catch
		C:E ->
			lager:error("~p~n~s", [M, lager:pr_stacktrace(erlang:get_stacktrace(), {C, E})]),
			erlang:error(E)
	end.

b2a(B) when is_atom(B) -> B;
b2a(B) -> erlang:binary_to_existing_atom(B, utf8).

typecast(Args) -> [ typecast_arg(Arg) || Arg <- Args ].
typecast_arg(Arg) when is_integer(Arg) -> erlang:integer_to_list(Arg);
typecast_arg(Arg) -> Arg.

replace_undefined(undefined) -> null;
replace_undefined(L) when is_list(L) -> [ replace_undefined(V) || V <- L ];
replace_undefined(M) when is_map(M) -> maps:from_list([ {K, replace_undefined(V)} || {K, V} <- maps:to_list(M) ]);
replace_undefined(V) -> V.

get_peer(Req) -> get_peer(maps:get(headers, Req, #{}), Req).
get_peer(#{ <<"x-real-ip">> := Peer }, _) -> Peer;
get_peer(#{ <<"x-forwarded-for">> := Peer }, _) -> Peer;
get_peer(_, #{ peer := Peer }) -> ws_misc:format_ip(Peer).
