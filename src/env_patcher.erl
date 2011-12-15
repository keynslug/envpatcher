-module(env_patcher).

-behaviour(supervisor).

-export([start/0, stop/0]).
-export([init/1]).

%% Application startup

start() ->
    start_dependencies(?MODULE),
    application:start(?MODULE).

stop() ->
    application:stop(?MODULE).

%% Callbacks

init(_) ->
    Ok = {ok, {{one_for_one, 1, 1}, []}},
    process_flag(trap_exit, true),
    try
        Globals = application:get_all_env(),
        Filename = deepprops:get([filename], Globals, "rules.config"),
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
    Sub = deepprops:get([Key], Options, []),
    case do_inject_options(Value, Sub, Gather) of
        [] ->
            Options;
        Injected when is_list(Injected) ->
            deepprops:set([Key], Injected, Options);
        Error ->
            Error
    end;

do_inject_options({Key, Value}, Options, Gather) ->
    Unique = make_ref(),
    case deepprops:get([Key], Options, Unique) of
        Unique ->
            do_inject_option(Key, Value, Options, Gather);
        _ ->
            Options
    end;

do_inject_options(Unexpected, _, _) ->
    {error, {unexpected_construct, Unexpected}}.

do_inject_option(Key, {ref, RefAppName, Path = [_ | _]}, Options, Gather) ->
    Unique = make_ref(),
    RefOptions = Gather(RefAppName),
    case deepprops:get(Path, RefOptions, Unique) of
        Unique -> 
            {error, {reference_undefined, RefAppName, Path}};
        Value ->
            deepprops:set([Key], Value, Options)
    end;

do_inject_option(Key, {clause, Clause, Value}, Options, Gather) ->
    case check_clause(Clause, Gather) of
        true -> 
            do_inject_option(Key, Value, Options, Gather);
        false ->
            Options;
        Error ->
            Error
    end;

do_inject_option(Key, {value, Value}, Options, _) ->
    deepprops:set([Key], Value, Options);

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
        _    -> {error, injection_failed}
    end.

check_clause({AppName, Path}, Gather) ->
    check_clause({AppName, Path, true}, Gather);

check_clause({AppName, Path = [_ | _], Value}, Gather) ->
    Unique = make_ref(),
    Options = Gather(AppName),
    case deepprops:get(Path, Options, Unique) of
        Value -> true;
        _     -> false
    end;

check_clause(Invalid, _) ->
    {error, {invalid_clause, Invalid}}.

shutdown({error, Error}) ->
    Format = "env_patcher: failed to inject options due to ~800p",
    Message = lists:flatten(io_lib:format(Format, [Error])),
    erlang:halt(Message).

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
        fun (App) -> deepprops:get(App, Was, []) end, 
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
    Gather = fun (App) -> deepprops:get(App, Was, []) end,
    Scatter = fun (_, _) -> ok end,
    ?assertEqual({error, {unexpected_construct, {ref, [enable]}}}, inject_options(Rules0, Gather, Scatter)),
    ?assertEqual({error, {unexpected_construct, "wharevah"}}, inject_options(Rules1, Gather, Scatter)),
    ?assertEqual({error, {reference_undefined, app2, [none]}}, inject_options(Rules2, Gather, Scatter)).


clause_test() ->
    
    Was = [
        {app1, []},
        {app2, [
            {some, [{story, true}, {line, false}]},
            {myname, {johnny, dillinger}},
            {wrong, false}
        ]},
        {core_app, [
            {prop, [
                {list, [
                    {entry, here}
                ]}
            ]}
        ]}
    ],
    
    Gather = fun (App) -> deepprops:get(App, Was, []) end,
    Rules = [
        {app1, [
            {enabled, {clause, {app2, [some, story]}, {value, true}}},
            {disabled, {clause, {app2, [wrong]}, {value, yeah}}},
            {where, {clause, {app2, [myname], {johnny, dillinger}}, {ref, core_app, [prop, list, entry]}}}
        ]}
    ],
    
    Result = inject_options(Rules, Gather, fun (app1, Options) -> 
        ?assertEqual([{where, here}, {enabled, true}], Options)
    end),
    ?assertEqual(ok, Result),
    
    Error = inject_options([ {app1, [ {some, {clause, {crap, there}, 42}} ]} ], Gather, fun (_, _) -> ok end),
    ?assertMatch({error, {invalid_clause, _}}, Error).


deep_clause_test() ->
    
    Was = [ {app1, []}, {app2, [ {wrong, false} ]} ],
    Gather = fun (App) -> deepprops:get(App, Was, []) end,
    Rules = [
        {app1, [
            {enabled, [{what, {clause, {app2, [wrong]}, {value, 42}}}]}
        ]}
    ],
    
    Result = inject_options(Rules, Gather, fun (app1, Options) -> 
        ?assertEqual([], Options)
    end),
    ?assertEqual(ok, Result).

-endif.
