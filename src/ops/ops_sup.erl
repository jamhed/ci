-module(ops_sup).
-behaviour(gen_server).

-export([start_link/0, start/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start(Data) ->
	gen_server:call(?MODULE, {start, Data}).

init([]) ->
	lager:notice("start", []),
	process_flag(trap_exit, true),
	{ok, #state{}}.

handle_cast(_Msg, S=#state{}) ->
	lager:error("unhandled cast:~p", [_Msg]),
	{noreply, S}.

handle_info({'EXIT', _Pid, normal}, S=#state{}) ->
	{noreply, S};

handle_info(_Info, S=#state{}) ->
	lager:error("unhandled info:~p", [_Info]),
	{noreply, S}.

handle_call({start, Data}, _From, S=#state{}) ->
	{ok, _Pid} = ops_job:start_link(Data),
	{reply, ok, S};

handle_call(_Request, _From, S=#state{}) ->
	lager:error("unhandled call:~p", [_Request]),
	{reply, ok, S}.

terminate(_Reason, _S) ->
	lager:notice("terminate, reason:~p", [_Reason]),
	ok.

code_change(_OldVsn, S=#state{}, _Extra) -> {ok, S}.
