-module(cmp_utils_test).

-include_lib("eunit/include/eunit.hrl").

-import(cmp_utils, [erlang_diff/2, 
                    erlang_diff_ll/2, 
                    diff_list/3, 
                    diff_tuple/3,
                    take_closest_tuple/2]).

d(One, Two) -> erlang_diff(One, Two).

erlang_diff_test_() ->
    [?_assertMatch('$same', d(a, a)),
     ?_assertMatch({'$changed', a, b}, d(a, b)),
     ?_assertMatch('$same', d([a], [a])),
     ?_assertMatch([{'$add', a}], d([1, b, c, d], [1, b, c, a, d])),
     ?_assertMatch([{'$add', {a, 1}}], d([{b, 1}], [{a, 1}, {b, 1}])),
     ?_assertMatch([{'$add', {a, 1}}], 
                   d([{b, 1}, {a, 2}], [{a, 2}, {a, 1}, {b, 1}])),
     ?_assertMatch({a, b, {'$add', c}}, d({a, b}, {a, b, c})),
     ?_assertMatch({a, {'$changed', b, c}}, d({a, b}, {a, c}))
    ].

diff_list_test_() ->
    [?_assertMatch([{a, {'$changed', 1, 2}}], diff_list([{a, 1}], [{a, 2}], [])),
     ?_assertMatch([{'$add', {a, 1}}], diff_list([{b, 1}], [{a, 1}, {b, 1}], []))
    ].

take_closest_test_() ->
    [?_assertMatch({value, {a, b, c}, []}, take_closest_tuple({a, b, c}, [{a, b, c}])),
     ?_assertMatch({value, {a, b, c}, [{a, b}]}, take_closest_tuple({a, b, c}, [{a, b}, {a, b, c}]))
    ].

