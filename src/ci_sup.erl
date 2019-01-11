-module(ci_sup).
-behaviour(supervisor).

-export([start_link/0, init/1, stop/1]).

-define(CHILD(I, M, A), #{ id => I, start => {M, start_link, A}, restart => permanent, type => worker }).
-define(CHILDS(I, M, A), #{ id => I, start => {M, start_link, A}, restart => permanent, type => supervisor }).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	lager:notice("start"),
	ci_db:setup(),
	Children = [
		?CHILDS(web_sup, web_sup, [])
	],
	{ok, { {one_for_one, 5, 10}, Children} }.

stop(_State) ->
	ok.
