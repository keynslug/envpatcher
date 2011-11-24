-module(env_patcher_apputils).

-include_lib("kernel/include/file.hrl").

-export([

    options/1,
    
    priv_dir/0,
    priv_dir/1,
    priv_path/1,
    
    start_dependencies/1

]).

%% Options

options({inherit, Name}) ->
    env_patcher_props:get([Name], application:get_all_env(), []);

options(global) ->
    application:get_all_env().

%% Private directories expansion

priv_dir() ->
    {ok, AppName} = application:get_application(),
    priv_dir(AppName).

priv_dir(AppName) ->
    case code:priv_dir(AppName) of
        Value when is_list(Value) ->
            Value ++ "/";
        _Error ->
            case file:read_file_info([priv]) of
                {ok, #file_info{type=directory}} ->
                    "priv/";
                _ ->
                    erlang:throw({error, failed_to_find_priv_dir})
            end
     end.

priv_path({AppName, [$/ | Relative]}) ->
    priv_dir(AppName) ++ Relative;

priv_path({AppName, Relative}) ->
    priv_dir(AppName) ++ Relative;

priv_path(Relative) ->
    {ok, AppName} = application:get_application(),
    priv_path({AppName, Relative}).

start_dependencies(AppName) ->
    Live = [ A || {A, _, _} <- application:which_applications() ],
    Deps = lists:reverse(gather_deps(AppName, []) -- [AppName | Live]),
    [ application:start(Dep) || Dep <- Deps ].

gather_deps(AppName, Acc) ->
    case lists:member(AppName, Acc) of
        true -> Acc;
        _    ->
            application:load(AppName),
            case application:get_key(AppName, applications) of
                {ok, DepsList} -> [AppName | lists:foldl(fun gather_deps/2, Acc, DepsList)];
                _              -> [AppName | Acc]
            end
    end.
