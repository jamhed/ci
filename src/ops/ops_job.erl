-module(ops_job).
-behaviour(gen_server).

-export([start_link/1, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
	data,
	user_id,
	repo_id,
	sha
}).

start_link(Data) -> gen_server:start_link(?MODULE, [Data], []).
stop(Pid) -> gen_server:cast(Pid, {stop}).

init([Data]) ->
	lager:notice("start: ~p", [js_push:repo_name(Data)]),
	{ok,
		#state{
			data=Data,
			user_id=js_push:sender_id(Data),
			repo_id=js_push:repo_id(Data),
			sha=js_push:commit_sha(Data)
		}
	}.

handle_cast({stop}, S=#state{}) ->
	{stop, normal, S};

handle_cast(_Msg, S=#state{}) ->
	lager:error("unhandled cast:~p", [_Msg]),
	{noreply, S}.

handle_info(_Info, S=#state{}) ->
	lager:error("unhandled info:~p", [_Info]),
	{noreply, S}.

handle_call(_Request, _From, S=#state{}) ->
	lager:error("unhandled call:~p", [_Request]),
	{reply, ok, S}.

terminate(_Reason, _S) ->
	lager:notice("terminate, reason:~p", [_Reason]),
	ok.

code_change(_OldVsn, S=#state{}, _Extra) -> {ok, S}.
