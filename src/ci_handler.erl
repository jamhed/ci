-module(ci_handler).
-export([init/2]).
-record(state, {}).

init(#{ method := <<"POST">> }=Req0, _InitState) ->
	{ok, Data, Req} = cowboy_req:read_body(Req0),
	Request = cowboy_req:header(<<"x-github-event">>, Req),
	handle_reply(handle_request(Request, jiffy:decode(Data, [return_maps])), Req, #state{}).

handle_request(<<"pull_request">>, Data) -> handle_pr(Data);
handle_request(<<"push">>, Data) -> handle_push(Data);
handle_request(Request, _) -> lager:warning("unhandled request:~p", [Request]).

handle_reply({ok, Data}, Req0, S=#state{}) ->
	{ok, cowboy_req:reply(200, #{}, Data, Req0), S};

handle_reply(ok, Req0, S=#state{}) ->
	{ok, cowboy_req:reply(404, #{}, <<>>, Req0), S}.

handle_push(Data) ->
	Repo = path([repository, full_name], Data),
	Branch = path([ref], Data),
	Commit = path(['after'], Data),
	handle_push(Repo, Branch, Commit, Data),
	{ok, <<"ok">>}.

handle_pr(Data) ->
	Repo = path([pull_request, base, repo, full_name], Data),
	Branch = path([pull_request, base, ref], Data),
	Action = path([action], Data),
	Pr = path([pull_request, number], Data),
	Commit = path([pull_request, head, sha], Data),
	handle_pr(Action, Repo, Branch, Pr, Commit, Data),
	{ok, <<"ok">>}.

handle_push(Repo, Branch, _Commit, _Data) ->
	lager:notice("push repo:~s branch:~s commit:~s", [Repo, Branch, _Commit]),
	exec:run("").

handle_pr(Action, Repo, Branch, Pr, _Commit, _Data) ->
	lager:notice("~s pr:~p repo:~s branch:~s", [Action, Pr, Repo, Branch]),
	exec:run("").

path(_, undefined) -> undefined;
path([], M) -> M;
path([K|Rest], M) -> path(Rest, maps:get(a2b(K), M, undefined)).

a2b(A) when is_atom(A) -> erlang:atom_to_binary(A, utf8);
a2b(A) -> A.

