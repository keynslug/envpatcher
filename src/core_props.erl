-module(core_props).

-export([
    get/2,
    get/3,
    set/3,
    extract/2, 
    extract/3, 
    append/3, 
    list/2, 
    values/2,
    split/2, 
    defaults/2
]).

%% Properties deep access

get(Path, Proplist) ->
    get(Path, Proplist, undefined).

get([], Value, _) ->
    Value;
get([Key | _], [Key | _], Default) ->
    Default;
get([Key | Rest], [{Key, Value} | _], Default) ->
    get(Rest, Value, Default);
get(Path, [_ | Left], Default) ->
    get(Path, Left, Default);
get(_, _, Default) ->
    Default.

set([], Entry, _) -> 
    Entry;
set([Key | Rest], Entry, Proplist = [{_, _} | _]) ->
    With = [ {K, set(Rest, Entry, V)} || P = {K, V} <- Proplist, keymatch(Key, P) ],
    Without = [ P || P <- Proplist, not keymatch(Key, P) ],
    case With of
        [E|_] -> [E | Without];
        []    -> [{Key, set(Rest, Entry, [])} | Without]
    end;
set([Key | Rest], Entry, Value) ->
    [{Key, set(Rest, Entry, Value)}].

append([], Entry, Acc) -> 
    [Entry | Acc];
append([Key | Rest], Entry, Proplist = [{_, _} | _]) ->
    With = [ {K, append(Rest, Entry, V)} || P = {K, V} <- Proplist, keymatch(Key, P) ],
    Without = [ P || P <- Proplist, not keymatch(Key, P) ],
    case With of
        [E|_] -> [E | Without];
        []    -> [{Key, append(Rest, Entry, [])} | Without]
    end;
append([Key | Rest], Entry, Value) ->
    [{Key, append(Rest, Entry, Value)}].

extract(Path, Proplist) ->
    extract(Path, Proplist, undefined).

extract([], Proplist, _) ->
    {Proplist, []};
extract([Key], Proplist = [{_, _} | _], Default) ->
    Values = [ V || P = {_, V} <- Proplist, keymatch(Key, P) ],
    Rest = [ P || P <- Proplist, not keymatch(Key, P) ],
    case Values of
        []       -> {Default, Rest};
        [Single] -> {Single, Rest};
        List     -> {List, Rest}
    end;
extract([Key | RestKeys], Proplist = [{_, _} | _], Default) ->
    WithIt = lists:append([ V || P = {_, V} <- Proplist, keymatch(Key, P) ]),
    Without = [ P || P <- Proplist, not keymatch(Key, P) ],
    {Value, Rest} = extract(RestKeys, WithIt, Default),
    {Value, [{Key, Rest} | Without]};
extract(_, Proplist, Default) ->
    {Default, Proplist}.

keymatch(Key, Key) -> true;
keymatch(Key, {Key, _}) -> true;
keymatch(_, _) -> false.

%% Properties multiple access

values(Keys, Proplist) ->
    values(Keys, Proplist, []).

values([], _, Acc) ->
    lists:reverse(Acc);
values([{Key, Default} | Rest], Proplist, Acc) ->
    values(Rest, Proplist, [get(keynormalize(Key), Proplist, Default) | Acc]);
values([Key | Rest], Proplist, Acc) ->
    values(Rest, Proplist, [get(keynormalize(Key), Proplist) | Acc]).

split(Keys, Proplist) ->
    do_split(Keys, {[], Proplist}).

do_split([], {Values, Rest}) ->
    {lists:reverse(Values), Rest};
do_split([{Key, Default} | Keys], {Acc, Proplist}) ->
    {Value, Rest} = extract(keynormalize(Key), Proplist, Default),
    do_split(Keys, {[Value | Acc], Rest});
do_split([Key | Keys], {Acc, Proplist}) ->
    {Value, Rest} = extract(keynormalize(Key), Proplist),
    do_split(Keys, {[Value | Acc], Rest}).

list(Keys, Proplist) ->
    list(Keys, Proplist, []).

list([Key | Rest], Proplist, Acc) ->
    Norm = keynormalize(Key),
    case get(Norm, Proplist) of
        undefined -> list(Rest, Proplist, Acc);
        Value     -> list(Rest, Proplist, [{Key, Value} | Acc])
    end;

list([], _, Acc) ->
    Acc.

keynormalize(Key) when is_list(Key) -> Key;
keynormalize(Key) -> [Key].

defaults([E = {Head, _} | Defaults], Proplist) ->
    case keydefined(Head, Proplist) of
        true -> defaults(Defaults, Proplist);
        _    -> defaults(Defaults, [E | Proplist])
    end;

defaults([Head | Defaults], Proplist) ->
    defaults([{Head, true} | Defaults], Proplist);

defaults([], Proplist) ->
    Proplist.

keydefined(Key, [Key | _])      -> true;
keydefined(Key, [{Key, _} | _]) -> true;
keydefined(Key, [_ | Rest])     -> keydefined(Key, Rest);
keydefined(_Key, [])            -> false.

%% Tests

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

do_test_() ->
    TestsCount = 13,
    [ ?_test(do_test(X)) || X <- lists:seq(1, TestsCount) ].

do_test(1) ->
    L = [ {top, [ {level, [ {thing, 2}, {where, 3} ]}, {middle, 4} ]}, {last, 5} ],
    R = [ {top, [ {level, [ {thing, new}, {where, 3} ]}, {middle, 4} ]}, {last, 5} ],
    ?assertEqual(R, set([top, level, thing], new, L));

do_test(2) ->
    L = [ {top, [ {level, [ {thing, 2}, {where, 3} ]}, {middle, 4} ]}, {last, 5} ],
    R = [ {top, [ {level, [ {thing2, [{thing3, new}]}, {thing, 2}, {where, 3} ]}, {middle, 4} ]}, {last, 5} ],
    ?assertEqual(R, set([top, level, thing2, thing3], new, L));

do_test(3) ->
    L = [ {top, [ {level, [ {thing2, [{thing3, new}]}, {thing, 2}, {where, 3} ]}, {middle, 4} ]}, {last, 5} ],
    R = [{thing3, new}],
    ?assertEqual(R, get([top, level, thing2], L));

do_test(4) ->
    L = [ {top, [ {level, [ {thing, 2}, {where, 3} ]}, {middle, 4} ]}, {last, 5} ],
    R = 4,
    ?assertEqual(R, get([top, middle], L));

do_test(5) ->
    L = [ {top, [ {level, [ {thing, 2}, {where, [3]} ]}, {middle, [4]} ]}, {last, 5} ],
    R = [ {top, [ {middle, [new, 4]}, {level, [ {thing, 2}, {where, [3]} ]} ]}, {last, 5} ],
    ?assertEqual(R, append([top, middle], new, L));

do_test(6) ->
    L = [ {top, [ {level, [ {thing, 2}, {where, 3} ]}, {middle, 4} ]}, {last, 5} ],
    R = undefined,
    ?assertEqual(R, get([top, middle, further], L));

do_test(7) ->
    L = [ {top, [ {level, [ {thing, 2}, {where, 3} ]} ]}, {last, 5} ],
    R = new,
    ?assertEqual(R, set([], new, L));

do_test(8) ->
    L = [ {level, 1}, {something, 2}, {somewhere, 3}, {last, 5} ],
    R = [ {someone, there}, {level, 1}, {something, 2}, {somewhere, 3}, {last, 5} ],
    Defs = [ {somewhere, none}, {someone, there} ],
    ?assertEqual(R, defaults(Defs, L));

do_test(9) ->
    L = [ {top, [ {level, [ {thing, 2}, {where, 3} ]}, {middle, 4} ]}, {last, 5} ],
    R = [ 2, def, 5 ],
    ?assertEqual(R, values([ [top, level, thing], {[top, down], def}, last ], L));

do_test(10) ->
    L = [ {top, [ {level, [ {thing, 2}, {where, 3} ]}, {middle, 4} ]}, {last, 5} ],
    R = {3, [ {top, [ {level, [ {thing, 2} ]}, {middle, 4} ]}, {last, 5} ]},
    ?assertEqual(R, extract([top, level, where], L));

do_test(11) ->
    L = [ {top, [ {level, [ {thing, 2}, {where, 3} ]}, {middle, 4} ]}, {last, 5} ],
    R = {L, []},
    ?assertEqual(R, extract([], L));

do_test(12) ->
    L = [ {top, [ {level, [ {thing, 2}, {where, 3} ]}, {middle, 4} ]}, {last, 5} ],
    R = {undefined, [ {top, [ {level, [ {where, 3}, {thing, 2} ]}, {middle, 4} ]}, {last, 5} ]},
    ?assertEqual(R, extract([top, level, where, does, it], L));

do_test(13) ->
    L = [ {top, [ {level, [ {thing, 2}, {where, 3} ]}, {middle, 4} ]}, {last, 5} ],
    R = {[ 2, def, 5 ], [ {top, [ {level, [ {where, 3} ]}, {middle, 4} ]} ]},
    ?assertEqual(R, split([ [top, level, thing], {[top, down], def}, last ], L)).

-endif.