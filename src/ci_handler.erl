-module(ci_handler).
-export([init/2]).
-record(state, {}).

fmt(Tmpl, Args) -> lists:flatten(io_lib:format(Tmpl, Args)).

pr_path(Number) when is_binary(Number) -> pr_path(erlang:binary_to_integer(Number));
pr_path(Number) when is_integer(Number) -> fmt("~s/pr/~p", [os:getenv("HOME"), Number]).
ci_path() -> fmt("~s/ci", [os:getenv("HOME")]).

init(#{ method := <<"GET">>, path := <<"/pr/", Pr/binary>> }=Req0, _InitState) ->
	Report = pr_path(Pr),
	lager:notice("get report:~p", [Report]),
	{ok, cowboy_req:reply(200, #{ <<"content-type">> => <<"text/plain">>}, {sendfile, 0, filelib:file_size(Report), Report}, Req0), _InitState};

init(#{ method := <<"POST">> }=Req0, _InitState) ->
	{ok, Data, Req} = cowboy_req:read_body(Req0),
	handle_reply(handle_data(jiffy:decode(Data, [return_maps])), Req, #state{}).

handle_reply({ok, Data}, Req0, S=#state{}) ->
	{ok, cowboy_req:reply(200, #{}, Data, Req0), S};

handle_reply(ok, Req0, S=#state{}) ->
	{ok, cowboy_req:reply(404, #{}, <<>>, Req0), S}.

handle_data(Data) ->
	Repo = path([pull_request, base, repo, name], Data),
	Action = path([action], Data),
	Pr = path([pull_request, number], Data),
	handle_action(Action, Repo, Pr, Data),
	{ok, <<"ok">>}.

handle_action(Action, <<"reach3">>, Pr, Data) when Action =:= <<"opened">>; Action =:= <<"synchronize">> ->
	Commit = path([pull_request, head, sha], Data),
	lager:notice("build and test reach3 pr:~p commit:~p", [Pr, Commit]),
	exec:run(fmt("cd ~s && ./build-pr.sh ~p ~s 2>&1", [ci_path(), Pr, Commit]), [{stdout, pr_path(Pr)}]),
	ok;

handle_action(<<"closed">>, <<"reach3">>, Pr, _Data) ->
	lager:notice("clear pr:~p", [Pr]),
	exec:run(fmt("cd ~s && ./clear-pr.sh ~p 2>&1", [ci_path(), Pr]), []),
	ok;

handle_action(Action, Repo, Pr, _Data) ->
	lager:notice("action:~p repo:~p pr:~p", [Action, Repo, Pr]).

path(_, undefined) -> undefined;
path([], M) -> M;
path([K|Rest], M) -> path(Rest, maps:get(a2b(K), M, undefined)).

a2b(A) when is_atom(A) -> erlang:atom_to_binary(A, utf8);
a2b(A) -> A.
