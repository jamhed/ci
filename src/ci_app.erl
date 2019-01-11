-module(ci_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) -> ci_sup:start_link().
stop(_State) -> ok.