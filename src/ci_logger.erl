-module(ci_logger).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, test/0]).

-record(state, {}).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

test() ->
	exec:run("lsss", [{stdout, whereis(ci_logger)}, {stderr, whereis(ci_logger)}]).

init([]) ->
	{ok, #state{}}.
handle_cast(_Msg, S=#state{}) -> {noreply, S}.
handle_info({stdout, _Pid, Msg}, S=#state{}) ->
	lager:info("stdout: ~s", [Msg]),
	{noreply, S};
handle_info({stderr, _Pid, Msg}, S=#state{}) ->
	lager:error("stderr: ~s", [Msg]),
	{noreply, S}.
handle_call(_Request, _From, S=#state{}) -> {reply, ok, S}.
terminate(_Reason, _S) -> ok.
code_change(_OldVsn, S=#state{}, _Extra) -> {ok, S}.