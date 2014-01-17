-module(cmp_utils_tests).

-include_lib("eunit/include/eunit.hrl").

-import(cmp_utils, [erlang_diff/2, 
                    erlang_diff_ll/2, 
                    diff_list/3, 
                    diff_tuple/3,
                    take_closest_tuple/2]).

d(One, Two) -> erlang_diff(One, Two).

-define(d(Diff, One, Two), ?_assertEqual(Diff, d(One, Two))).

erlang_diff_test_() ->
    [?d('$same', a, a),
     ?d({'$changed', a, b}, a, b),
     ?d('$same', [a], [a]),
     ?d([{'$add', a}], [1, b, c, d], [1, b, c, a, d]),
     ?d([{'$add', {a, 1}}], [{b, 1}], [{a, 1}, {b, 1}]),
     ?d([{'$add', {a, 1}}], [{b, 1}, {a, 2}], [{a, 2}, {a, 1}, {b, 1}]),
     ?d({a, b, {'$add', c}}, {a, b}, {a, b, c}),
     ?d({a, {'$changed', b, c}}, {a, b}, {a, c})
    ].

diff_list_test_() ->
    [?_assertEqual([{a, {'$changed', 1, 2}}], diff_list([{a, 1}], [{a, 2}], [])),
     ?_assertEqual([{'$add', {a, 1}}], diff_list([{b, 1}], [{a, 1}, {b, 1}], []))
    ].

take_closest_test_() ->
    [?_assertEqual({value, {a, b, c}, []}, take_closest_tuple({a, b, c}, [{a, b, c}])),
     ?_assertEqual({value, {a, b, c}, [{a, b}]}, take_closest_tuple({a, b, c}, [{a, b}, {a, b, c}]))
    ].


tuple_test_() ->
    [?d({a, b, c, {'$add', d}}, {a, b, c}, {a, b, c, d}),
     ?d({a, b, {'$changed', d, c}, {'$add', d}}, {a, b, d}, {a, b, c, d}),
     ?d({a, b, {'$rm', c}}, {a, b, c}, {a, b})
    ].

list_hard_test_() ->
    F = fun (A, X) -> {A, X, X, X, X, X} end,
    [?d('$same', [F(a, 1)], [F(a, 1)]),
     ?d([F({'$changed', a, b}, no)], [F(a, no)], [F(b, no)]),
     ?d([F({'$changed', a, b}, no)], 
        [F(a, no), F(b, aaa)],
        [F(b, aaa), F(b, no)]),
     ?d([F({'$changed', a, b}, aaa)], 
        [F(a, aaa), F(b, no)],
        [F(b, aaa), F(b, no)]),
     ?d([{'$rm', F(a, aaa)}, F(b, {'$changed', no, no1})],
        [F(a, aaa), F(b, no)],
        [F(b, no1)]),
     ?d([{'$rm', F(a, aaa)}, F(b, {'$changed', no, no1})],
        [F(a, aaa), F(b, no)],
        [F(b, no1)]),
     ?d([{'$rm', F(a, a)}, {'$add', F(b, b)}, F(c, {'$changed', c, c1})],
        [F(a, a), F(c, c)],
        [F(b, b), F(c, c1)])
    ].

fixture(Nodes1, Nodes2) ->
    [{"VR rings",
      [{echo_view_cluster,default,[],
        ['echo-general','echo-general-2'],
        {subsystem,vr_cluster,
         Nodes1,
         [node()],
         [{lru_size,7000}],
         [{check_view_bulk,{vc1r0,"check_view:default"}},
          {call_vrc_async,{vc1r0,"view_router_async:default"}},
          {register_view_async,{vc1r0,"register_view:default"}}]},
        {subsystem,vu_cluster,
         Nodes2,
         [node()],
         [{subscribers_per_node,4},{ports_per_node,8}],
         [{default,{vc1r1,":default"}},{bulk,{vc1r1,":default"}}]},
        {subsystem,lu_cluster,
         [node()],
         [node()],
         [],
         [{default,{lu1r,":default"}}]},
        {views_caches,[mb_views],
         [mb_view_props],
         [mb_view_slices],
         [mb_active_views]},
        [{get_view_slice,[{max_queue_len,100},
                          {max_queue_per_key_len,100}]},
         {get_view_counter,[{max_queue_len,100},
                            {max_queue_per_key_len,100}]}]}]},
     {"VR has view_router role",true},
     {"VR started",true},
     {"VR AMQ params",
      [{check_view_bulk,{vc1r0,"check_view:default"}},
       {call_vrc_async,{vc1r0,"view_router_async:default"}},
       {register_view_async,{vc1r0,"register_view:default"}}]},
     {"VR params",[{lru_size,7000}]},
     {"VR ring",[node()]},
     {"VR active nodes",[node()]},
     {"Slave VR started",true},
     {"Standby VR started",true}].

real_test_() ->
    [?d( [{"VR rings",
           [{echo_view_cluster,default,[],
             ['echo-general','echo-general-2'],
             {subsystem,vr_cluster,
              [{'$rm',a},{'$add',b}],
              [node()],
              [{lru_size,7000}],
              [{call_vrc_async,{vc1r0,"view_router_async:default"}},
               {check_view_bulk,{vc1r0,"check_view:default"}},
               {register_view_async,{vc1r0,"register_view:default"}}]},
             {subsystem,vu_cluster,
              [b],
              [node()],
              [{ports_per_node,8},{subscribers_per_node,4}],
              [{bulk,{vc1r1,":default"}},{default,{vc1r1,":default"}}]},
             {subsystem,lu_cluster,
              [node()],
              [node()],
              [],
              [{default,{lu1r,":default"}}]},
             {views_caches,
              [mb_views],
              [mb_view_props],
              [mb_view_slices],
              [mb_active_views]},
             [{get_view_counter,[{max_queue_len,100},{max_queue_per_key_len,100}]},
              {get_view_slice,
               [{max_queue_len,100},{max_queue_per_key_len,100}]}]}]}] ,
        fixture([a], [b]),
        fixture([b], [b]))
    ].
