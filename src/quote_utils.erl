%%%
%%% Copyright (c) 2013 JackNyfe. All rights reserved.
%%% THIS SOFTWARE IS PROPRIETARY AND CONFIDENTIAL. DO NOT REDISTRIBUTE.
%%%
%%% vim: set ts=4 sts=4 sw=4 et:

-module(quote_utils).

-export([
    quote/1,
    simpleObject/1,
    smartQuotedValue/1,
    unquote/1,
    unquote/2
]).

simpleObject(List) when is_list(List) ->
    ["{ ", string_utils:intersperse(", ",
        [
            case V of
                [{_, _} | _] -> [quotedValue(K), ": ", simpleObject(V)];
                _ -> [quotedValue(K), ": ", smartQuotedValue(V)]
                end
            || {K, V} <- List
        ]
    ), " }"];
simpleObject(Value) -> smartQuotedValue(Value).

quotedValue(Value) when is_list(Value) ->
    [$" | lists:reverse(quotedValue(Value, []), "\"")];
quotedValue(Value) when is_atom(Value) -> quotedValue(atom_to_list(Value));
quotedValue(Value) when is_binary(Value) -> quotedValue(binary_to_list(Value));
quotedValue(Value) when is_integer(Value) -> quotedValue(integer_to_list(Value)).
quotedValue([$\\|T], Acc) -> quotedValue(T, [$\\,$\\|Acc]);
quotedValue([$'|T], Acc) -> quotedValue(T, [$',$\\|Acc]);
quotedValue([$"|T], Acc) -> quotedValue(T, [$",$\\|Acc]);
quotedValue([$\n|T], Acc) -> quotedValue(T, [$n,$\\|Acc]);
quotedValue([$\r|T], Acc) -> quotedValue(T, [$r,$\\|Acc]);
quotedValue([$\t|T], Acc) -> quotedValue(T, [$t,$\\|Acc]);
quotedValue([C|T], Acc) -> quotedValue(T, [C|Acc]);
quotedValue([], Acc) -> Acc.

smartQuotedValue(true) -> "true";
smartQuotedValue(false) -> "false";
smartQuotedValue(null) -> "null";
smartQuotedValue(undefined) -> "undefined";
smartQuotedValue({list, L}) -> ["[", string_utils:intersperse(", ", lists:map(fun simpleObject/1, L)), "]"];
smartQuotedValue(Value) when is_integer(Value) -> integer_to_list(Value);
smartQuotedValue(Value) -> quotedValue(Value).

quote(Value) -> lists:reverse(quotedValue(Value, [])).

unquote(Value) -> lists:reverse(unquote(Value, [])).

unquote([$\\,$t|T], Acc) -> unquote(T, [$\t|Acc]);
unquote([$\\,$n|T], Acc) -> unquote(T, [$\n|Acc]);
unquote([$\\,$r|T], Acc) -> unquote(T, [$\r|Acc]);
unquote([$\\,$'|T], Acc) -> unquote(T, [$'|Acc]);
unquote([$\\,$"|T], Acc) -> unquote(T, [$"|Acc]);
unquote([$\\,$\\|T], Acc) -> unquote(T, [$\\|Acc]);
unquote([C|T], Acc) -> unquote(T, [C|Acc]);
unquote([], Acc) -> Acc.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

quote_test_() ->
    [?_assertEqual(R, quotedValue(I)) || {R, I} <- [
        {"\"nothing\"", nothing},
        {"\"true\"", true},
        {"\"\"", ""},
        {"\"\\\"\"", "\""},
        {"\"\\n\"", "\n"}
    ]].

unqoute_test_() ->
    [?_assertEqual(R, unquote(I)) || {R, I} <- [
        {"", ""},
        {"a", "a"},
        {"a'", "a'"},
        {"a'", "a\\'"},
        {"a\"b", "a\\\"b"},
        {"a\"b'c", "a\\\"b\\'c"},
        {"a\"b'c\n", "a\\\"b\\'c\\n"}
    ]].

object_test_() ->
    ?_assertEqual("{ \"a\": 1, \"b\": 2, \"c\": { \"c1\": 1, \"c2\": 2, \"c3\": "
    "{ \"c31\": 1, \"c32\": 2 } } }",
        lists:flatten(simpleObject([
            {a, 1},
            {b, 2},
            {c, [
                {c1, 1},
                {c2, 2},
                {c3, [{c31, 1}, {c32, 2}]}
            ]}
        ]))
    ),

    ?_assertEqual("{ \"a\": 1, \"b\": 2, \"c\": undefined, \"d\": \"\" }",
        lists:flatten(simpleObject([
            {a, 1},
            {b, 2},
            {c, undefined},
            {d, ""}
        ]))
    ),

    % check for lists as values
    ?_assertEqual("{ "
    "\"a\": [{ \"bb\": 2, \"cc\": 3 }, { \"dd\": 4, \"ee\": 5 }], "
    "\"f\": [{ \"gg\": 6 }, { \"hh\": 7 }], "
    "\"i\": [{ \"jj\": 8, \"kk\": 9 }], "
    "\"l\": [{ \"mm\": 10 }], "
    "\"n\": []"
    " }",
        lists:flatten(simpleObject([
            {a, {list, [[{bb, 2}, {cc, 3}], [{dd, 4}, {ee, 5}]]}},
            {f, {list, [[{gg, 6}], [{hh, 7}]]}},
            {i, {list, [[{jj, 8}, {kk, 9}]]}},
            {l, {list, [[{mm, 10}]]}},
            {n, {list, []}}
        ]))
    ),

    % check for plain values in lists
    ?_assertEqual("{ "
    "\"a\": [{ \"bb\": 2 }, \"cc\"], "
    "\"d\": [\"ee\", \"ff\", 3], "
    "\"g\": [\"hh\"] "
    "}",
        lists:flatten(simpleObject([
            {a, {list, [[{bb, 2}], cc]}},
            {d, {list, [ee, ff, 3]}},
            {g, {list, [hh]}}
        ]))
    ).

-endif.
