-module(env_patcher).

-behaviour(supervisor).

-export([start/0, stop/0]).
-export([init/1]).

%% Application startup

start() ->
    core_apputils:start_dependencies(?MODULE),
    application:start(?MODULE).

stop() ->
    application:stop(?MODULE).

%% Callbacks

init(_) ->
    Ok = {ok, {{one_for_one, 1, 1}, []}},
    process_flag(trap_exit, true),
    try
        Globals = core_apputils:options(global),
        Filename = core_props:get([filename], Globals, "rules.config"),
        Rules = load_rules(Filename),
        ok = inject_options(Rules),
        Ok
    catch _:E ->
        shutdown(E)
    end.

%% Internals

load_rules(Filename) ->
    case file:consult(Filename) of
        {ok, Terms} -> Terms;
        Error       -> throw(Error)
    end.

inject_options(Rules) ->
    case inject_options(Rules, fun gather_options/1, fun scatter_options/2) of
        ok -> ok;
        Error -> throw(Error)
    end.

inject_options([], _, _) ->
    ok;

inject_options([Rule | Options], Gather, Scatter) ->
    case inject_options(Rule, Gather, Scatter) of
        ok -> inject_options(Options, Gather, Scatter);
        Error -> Error
    end;

inject_options({AppName, Refs}, Gather, Scatter) ->
    Original = Gather(AppName),
    case do_inject_options(Refs, Original, Gather) of
        New when is_list(New) ->
            Scatter(AppName, New);
        Error ->
            Error
    end.

do_inject_options(_, Error = {error, _}, _) ->
    Error;

do_inject_options([], Options, _) ->
    Options;

do_inject_options([{Key, Value} | Refs], Options, Gather) ->
    Injected = do_inject_options({Key, Value}, Options, Gather),
    do_inject_options(Refs, Injected, Gather);

do_inject_options({_Key, []}, Options, _) ->
    Options;

do_inject_options({Key, Value = [{_, _} | _]}, Options, Gather) ->
    Sub = core_props:get([Key], Options, []),
    case do_inject_options(Value, Sub, Gather) of
        Injected when is_list(Injected) ->
            core_props:set([Key], Injected, Options);
        Error ->
            Error
    end;

do_inject_options({Key, Value}, Options, Gather) ->
    case core_props:get([Key], Options, surely_undefined) of
        surely_undefined ->
            do_inject_option(Key, Value, Options, Gather);
        _ ->
            Options
    end;

do_inject_options(Unexpected, _, _) ->
    {error, {unexpected_construct, Unexpected}}.

do_inject_option(Key, {ref, RefAppName, Path = [_ | _]}, Options, Gather) ->
    RefOptions = Gather(RefAppName),
    case core_props:get(Path, RefOptions, surely_undefined) of
        surely_undefined -> 
            {error, {reference_undefined, RefAppName, Path}};
        Value ->
            core_props:set([Key], Value, Options)
    end;

do_inject_option(Key, {value, Value}, Options, _) ->
    core_props:set([Key], Value, Options);

do_inject_option(_Key, Unexpected, _, _) ->
    {error, {unexpected_construct, Unexpected}}.

gather_options(AppName) ->
    catch application:load(AppName),
    application:get_all_env(AppName).

scatter_options(AppName, Options) ->
    catch application:load(AppName),
    Result = [ application:set_env(AppName, Key, Value) || {Key, Value} <- Options ],
    case lists:all(fun (E) -> E =:= ok end, Result) of
        true -> ok;
        false -> {error, injection_failed}
    end.

shutdown({error, Error}) ->
    Format = "env_patcher: failed to inject options due to ~800p",
    Message = lists:flatten(io_lib:format(Format, [Error])),
    erlang:halt(Message).

%% Tests

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

injection_test() ->
    
    %% assume that is the spherical app.config
    Was = [
        {app1, []},
        {app2, [
            {untouched, true}
        ]},
        {core_app, [
            {enable, false},
            {override, true},
            {deep, [
                {prop, [
                    {list, [
                        {entry, here}
                    ]},
                    {empty, []}
                ]}
            ]}
        ]}
    ],
    
    %% there are the rules
    Rules = [
        {app1, [
            {enabled, {ref, core_app, [enable]}}
        ]},
        {app2, [
            {overriden, {ref, core_app, [override]}},
            {now, {ref, core_app, [deep, prop, list, entry]}},
            {new, {value, one}},
            {deep, [
                {prop, [
                    {list, {ref, core_app, [deep, prop, empty]}}
                ]}
            ]}
        ]}
    ],
    
    Result = inject_options(
        Rules, 
        fun (App) -> core_props:get([App], Was, []) end, 
        fun 
            (app1, Options) -> ?assertEqual([{enabled, false}], Options);
            (app2, Options) -> ?assertEqual([
                {deep, [
                    {prop, [
                        {list, []}
                    ]}
                ]},
                {new, one},
                {now, here},
                {overriden, true},
                {untouched, true}
            ], Options)
        end
    ),
    
    ?assertEqual(ok, Result).

failures_test() ->
    
    Rules0 = [
        {app1, [
            {enabled, {ref, [enable]}}
        ]}
    ],
    Rules1 = [
        {app1, [
            {thing, "wharevah"}
        ]}
    ],
    Rules2 = [
        {app1, [
            {some, {ref, app2, [none]}}
        ]}
    ],
    
    Was = [ {app1, []}, {app2, []} ],
    Gather = fun (App) -> core_props:get([App], Was, []) end,
    Scatter = fun (_, _) -> ok end,
    ?assertEqual({error, {unexpected_construct, {ref, [enable]}}}, inject_options(Rules0, Gather, Scatter)),
    ?assertEqual({error, {unexpected_construct, "wharevah"}}, inject_options(Rules1, Gather, Scatter)),
    ?assertEqual({error, {reference_undefined, app2, [none]}}, inject_options(Rules2, Gather, Scatter)).

-endif.
