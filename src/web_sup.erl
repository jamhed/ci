-module(web_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	{ok, _} = cowboy:start_clear(ci_http_listener,
		[{port, 8080}],
		#{env => #{dispatch => dispatch_rules()}}
	),
	{ok, {{one_for_one, 5, 10}, []}}.

dispatch_rules() ->
	cowboy_router:compile([
		{'_', [
			{"/", cowboy_static, {priv_file, ci, "index.html"}},
			{"/github", github_handler, []},
			{"/ws", ws_main, []},
			{"/static/[...]", cowboy_static, {priv_dir, ci, "static"}}
		]}
	]).
