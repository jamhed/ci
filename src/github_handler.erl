-module(github_handler).
-export([init/2]).
-record(state, {}).

init(#{ method := <<"POST">> }=Req0, _InitState) ->
	{ok, Data, Req} = cowboy_req:read_body(Req0),
	Event = cowboy_req:header(<<"x-github-event">>, Req),
	handle_reply(handle_request(Event, jiffy:decode(Data, [return_maps])), Req, #state{}).

handle_request(<<"push">>, Data) -> handle_push(Data);
handle_request(Event, _) ->
	lager:warning("unhandled event:~p", [Event]),
	{ok, <<"undefined">>}.

handle_reply({ok, Data}, Req0, S=#state{}) ->
	{ok, cowboy_req:reply(200, #{}, Data, Req0), S};

handle_reply(ok, Req0, S=#state{}) ->
	{ok, cowboy_req:reply(404, #{}, <<>>, Req0), S}.

handle_push(Data) ->
	github_db:handle_push(Data),
	{ok, <<"ok">>}.
