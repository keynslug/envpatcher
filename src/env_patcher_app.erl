-module(env_patcher_app).

-behaviour(application).

-export([start/0]).
-export([start/2, stop/1]).

start() ->
    application:start(env_patcher).

start(_StartType, _StartArgs) ->
    supervisor:start_link({local, ?MODULE}, env_patcher, []).

stop(_) ->
    ok.
