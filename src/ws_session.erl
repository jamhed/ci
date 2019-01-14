-module(ws_session).
-behaviour(gen_server).
-export([start_link/0, register/1, get/1, list/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
	sessions = #{},
	monitors = #{}
}).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

register(Data) -> gen_server:call(?MODULE, {register, self(), Data}).
get(UUID) -> gen_server:call(?MODULE, {get, UUID}).
list() -> gen_server:call(?MODULE, {list}).

init([]) ->
	lager:notice("start", []),
	erlang:send_after(1000, self(), swipe),
	{ok, #state{}}.

handle_cast(_Msg, S=#state{}) ->
	lager:error("unhandled cast:~p", [_Msg]),
	{noreply, S}.

handle_info({'DOWN', Ref, process, _Pid, _Reason}, S=#state{monitors=Monitors, sessions=Sessions}) ->
	case maps:get(Ref, Monitors, undefined) of
		undefined -> {noreply, S};
		UUID -> {noreply, S#state{monitors=maps:remove(UUID, Monitors), sessions=update(UUID, Sessions)}}
	end;

handle_info(swipe, S=#state{sessions=Sessions}) ->
	erlang:send_after(1000, self(), swipe),
	{noreply, S#state{sessions=swipe(Sessions)}};

handle_info(_Info, S=#state{}) ->
	lager:error("unhandled info:~p", [_Info]),
	{noreply, S}.

handle_call({register, Pid, Data}, _From, S=#state{monitors=Monitors, sessions=Sessions}) ->
	UUID = uuid(),
	Ref = erlang:monitor(process, Pid),
	{reply, UUID, S#state{monitors=Monitors#{ Ref => UUID }, sessions=Sessions#{ UUID => {keep, Data} }}};

handle_call({get, UUID}, _From, S=#state{sessions=Sessions}) ->
	Re = 
		case maps:get(UUID, Sessions, undefined) of
			{keep, _} -> logged_in;
			{_, Data} -> Data;
			_ -> not_exists
		end,
	{reply, Re, S};

handle_call({list}, _From, S=#state{sessions=Sessions}) ->
	{reply, Sessions, S};

handle_call(_Request, _From, S=#state{}) ->
	lager:error("unhandled call:~p", [_Request]),
	{reply, ok, S}.

terminate(_Reason, _S) ->
	lager:notice("terminate, reason:~p", [_Reason]),
	ok.

code_change(_OldVsn, S=#state{}, _Extra) ->
	{ok, S}.

uuid() -> erlang:list_to_binary(uuid:uuid_to_string(uuid:get_v5(<<"ws_sessions">>, uuid:get_v4()))).

ts() ->
	erlang:monotonic_time(seconds).

swipe(M=#{}) ->
	Timeout = 60,
	UUIDs = [ UUID || {UUID, {Ts, _}} <- maps:to_list(M), Ts =/= keep, ts() - Ts > Timeout ],
	lists:foldl(fun(UUID, Acc) -> maps:remove(UUID, Acc) end, M, UUIDs).

update(UUID, Sessions) ->
	case maps:get(UUID, Sessions, undefined) of
		{_, Data} -> Sessions#{ UUID => {ts(), Data} };
		_ -> Sessions
	end.
