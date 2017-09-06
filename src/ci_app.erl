-module(ci_app).
-behaviour(application).

-export([start/2, stop/1]).

dispatch_rules() ->
	cowboy_router:compile([
		{'_', [
			{"/[...]", ci_handler, []}
		]}
	]).

start(_Type, _Args) ->
	cowboy:start_clear(my_http_listener,
		[{port, 8088}],
		#{env => #{dispatch => dispatch_rules()}}
	).

stop(_State) ->
	ok.
