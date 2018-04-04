-module(ci_handler).
-export([init/2, path/0]).
-record(state, {}).

fmt(Tmpl, Args) -> lists:flatten(io_lib:format(Tmpl, Args)).

ci_path() -> fmt("~s/ci", [os:getenv("HOME")]).
path() -> fmt("~s/reports", [os:getenv("HOME")]).
path(Commit) -> fmt("~s/reports/~s", [os:getenv("HOME"), Commit]).

init(#{ method := <<"GET">>, path := <<"/reports/", Commit/binary>> }=Req0, _InitState) ->
	Report = path(Commit),
	lager:notice("get report:~p", [Report]),
	{ok, cowboy_req:reply(200, #{ <<"content-type">> => <<"text/plain">>}, {sendfile, 0, filelib:file_size(Report), Report}, Req0), _InitState};

init(#{ method := <<"POST">> }=Req0, _InitState) ->
	{ok, Data, Req} = cowboy_req:read_body(Req0),
	Request = cowboy_req:header(<<"x-github-event">>, Req),
	handle_reply(handle_request(Request, jiffy:decode(Data, [return_maps])), Req, #state{}).

handle_request(<<"pull_request">>, Data) ->
	handle_pr(Data);
handle_request(<<"push">>, Data) ->
	handle_push(Data);
handle_request(Request, _) ->
	lager:warning("unhandled request:~p", [Request]).

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
	handle_pr(Action, Repo, Pr, Commit, Branch, Data),
	{ok, <<"ok">>}.

handle_push(Repo, Branch, Commit, _Data) ->
	lager:notice("push repo:~s branch:~s commit:~s", [Repo, Branch, Commit]),
	exec:run(fmt("cd ~s && ./handle-push.sh ~s ~s ~s", [ci_path(), Repo, Branch, Commit]), make_writer(Commit)).

handle_pr(Action, Repo, Branch, Pr, Commit, _Data) ->
	lager:notice("~s pr:~p repo:~s branch:~s", [Action, Pr, Repo, Branch]),
	exec:run(fmt("cd ~s && ./handle-pr.sh ~s ~p ~s ~s ~s", [ci_path(), Action, Pr, Repo, Branch, Commit]), make_writer(Commit)).

path(_, undefined) -> undefined;
path([], M) -> M;
path([K|Rest], M) -> path(Rest, maps:get(a2b(K), M, undefined)).

a2b(A) when is_atom(A) -> erlang:atom_to_binary(A, utf8);
a2b(A) -> A.

make_writer(Commit) ->
	Writer = writer(Commit),
	[{stderr, Writer}, {stdout, Writer}].

writer(Commit) ->
	fun(Stream, _OsPid, Data) ->
		log(Stream, Data),
		{ok, Device} = file:open(path(Commit), [append, raw]),
		file:write(Device, Data),
		file:close(Device)
	end.

chomp(Str) when is_binary(Str) ->
	binary:part(Str, {0, erlang:size(Str)-1}).

log(stdout, Data) ->
	[ lager:info("~s", [Line]) || Line <- binary:split(chomp(Data), <<"\n">>, [global]) ];
log(stderr, Data) ->
	[ lager:error("~s", [Line]) || Line <- binary:split(chomp(Data), <<"\n">>, [global]) ].
