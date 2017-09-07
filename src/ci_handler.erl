-module(ci_handler).
-export([init/2]).
-record(state, {}).

init(Req0, _InitState) ->
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

handle_action(<<"open">>, <<"reach3">>, Pr, _Data) ->
	ok;

handle_action(Action, Repo, Pr, _Data) ->
	lager:notice("action:~p repo:~p pr:~p", [Action, Repo, Pr]).

path(_, undefined) -> undefined;
path([], M) -> M;
path([K|Rest], M) -> path(Rest, maps:get(a2b(K), M, undefined)).

a2b(A) when is_atom(A) -> erlang:atom_to_binary(A, utf8);
a2b(A) -> A.
