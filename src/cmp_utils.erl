-module(cmp_utils).

-export([diff/2,
         set_diff/1,
         get_diff/0,
         cluster/1,
         diff_one_to_many/2]).

-export([pdiff/2,
         pdiff_one_to_many/2]).

-export([system_diff/2]).
-export([erlang_diff/2]).

-ifdef(TEST).
-export([erlang_diff_ll/2,
         diff_list/3,
         diff_tuple/3,
         take_closest_tuple/2]).
-endif.

set_diff(Name) when Name =:= system; Name =:= erlang ->
    put('$use_diff', Name).

get_diff() ->
    case get('$use_diff') of
        undefined -> 
            case application:get_env(utils, use_diff) of
                undefined -> erlang;
                Value -> Value
            end;
        Value -> Value
    end.

diff(Term1, Term2) ->
    case get_diff() of
        erlang -> erlang_diff(Term1, Term2);
        system -> system_diff(Term1, Term2)
    end.

pdiff(Diff) ->
    case get_diff() of
        erlang -> io:format("~p~n", [Diff]);
        system -> io:format("~s~n", [Diff])
    end.

pdiff(Term1, Term2) ->
    pdiff(diff(Term1, Term2)).



-type cluster_tag() :: term().
-type cluster_value() :: term().
-spec cluster(Pairs :: [{cluster_tag(), cluster_value()}]) 
    -> [{[cluster_tag()], cluster_value()}].
cluster(Pairs) when is_list(Pairs) ->
    FlippedPairs = [{sort_everything(Value), Key} || {Key, Value} <- Pairs],
    SPairs = lists:reverse(lists:usort(FlippedPairs)),
    lists:foldl(fun
            ({Value, Key}, [{Keys, Value} | Tail]) ->
                [{[Key | Keys], Value} | Tail];
            ({Value, Key}, Tail) ->
                [{[Key], Value} | Tail]
        end, [], SPairs).

diff_one_to_many(One, Many) when is_list(Many) ->
    ClusteredMany = cluster(Many),
    [{Keys, diff(One, Value)} || {Keys, Value} <- ClusteredMany].

pdiff_one_to_many(One, Many) ->
    io:format("Origin:~n~p~n", [One]),
    io:format("===============================~n"),
    Diffs = diff_one_to_many(One, Many),
    ClusterCount = length(Diffs),
    SortDiffs = lists:sort( fun 
                ({Fst, _}, {Snd, _}) ->
                    length(Fst) > length(Snd)
            end, Diffs),
    lists:zipwith(fun
            ({Keys, Diff}, Number) ->
                io:format("~nGroup ~B: ~p~n"
                          "Diff:~n", [Number, Keys]),
                          
                pdiff(Diff),
                io:format("===============================~n")
        end, SortDiffs, lists:seq(1, ClusterCount)),
    ok.
                

%%
%% Various diffs
%%
system_diff(Term1, Term2) ->
    Sorted1 = sort_everything(Term1),
    Sorted2 = sort_everything(Term2),
    OutputFile1 = tmpfile(),
    OutputFile2 = tmpfile(),
    file:write_file(OutputFile1, io_lib:fwrite("~p\n", [Sorted1])),
    file:write_file(OutputFile2, io_lib:fwrite("~p\n", [Sorted2])),
    os:cmd("diff " ++ OutputFile1 ++ " " ++ OutputFile2).


erlang_diff(Term1, Term2) ->
    Sorted1 = sort_everything(Term1),
    Sorted2 = sort_everything(Term2),
    case Sorted1 of
        Sorted2 -> '$same';
        _ -> erlang_diff_ll(Sorted1, Sorted2)
    end.


%%
%% Internals
%%

erlang_diff_ll(Same, Same) ->
    Len = lists:flatlength(io_lib:fwrite("~p", [Same])),
    if
        Len < 16 -> Same;
        true -> '$same'
    end;
erlang_diff_ll(List1, List2) 
        when is_list(List1), is_list(List2) ->
    case io_lib:printable_list(List1) 
        andalso io_lib:printable_list(List2) of
        true ->
            {'$changed', List1, List2};
        _ ->
            diff_list(List1 -- List2, List2 -- List1, [])
    end;
erlang_diff_ll(Tuple1, Tuple2)
        when is_tuple(Tuple1), is_tuple(Tuple2) ->
    diff_tuple(tuple_to_list(Tuple1), tuple_to_list(Tuple2), []);
erlang_diff_ll(One, Two) ->
    {'$changed', One, Two}.


diff_list(One, One, Acc) ->
    lists:reverse(Acc);
diff_list([H1 | T1], [H1 | T2], Acc) ->
    diff_list(T1, T2, Acc);
diff_list([H1 | T1], [], Acc) ->
    diff_list(T1, [], [{'$rm', H1} | Acc]);
diff_list([], [H2 | T2], Acc) ->
    diff_list([], T2, [{'$add', H2} | Acc]);
diff_list([H1 | T1] = L1, [H2 | T2] = L2, Acc) 
        when is_tuple(H1), is_tuple(H2) ->
    CompanionTo1 = take_closest_tuple(H1, L2),
    CompanionTo2 = take_closest_tuple(H2, L1),
    case {CompanionTo1, CompanionTo2} of
        {{value, H2, Tt2}, {value, H1, Tt1}} ->
            NewAcc = diff_tuple_in_list(H1, H2) ++ Acc,
            diff_list(Tt1, Tt2, NewAcc);
        {{value, C1, Tt2}, {value, C2, Tt1}} -> 
            One = diff_tuple_in_list(H1, C1),
            Two = diff_tuple_in_list(C2, H2),
            NewAcc = (Two ++ One) ++ Acc,
            diff_list(Tt1, Tt2, NewAcc);
        {false, false} ->
            One = {'$rm', H1},
            Two = {'$add', H2},
            NewAcc = [Two | [One | Acc]],
            diff_list(T1, T2, NewAcc);
        {{value, C1, Tt2}, false} ->
            NewAcc = [{'$add', H2} | diff_tuple_in_list(H1, C1)] ++ Acc,
            io:format("T1 = ~p, Tt2 = ~p~n", [T1, Tt2]),
            diff_list(T1, without(H2, Tt2), NewAcc);
        {false, {value, C2, Tt1}} ->
            NewAcc = diff_tuple_in_list(C2, H2) ++ [{'$rm', H1} | Acc],
            diff_list(without(H1, Tt1), T2, NewAcc)
    end;
diff_list([H1 | T1], [H2 | T2], Acc) 
        when is_list(H1), is_list(H2) ->
    NewAcc = [erlang_diff_ll(H1, H2) | Acc],
    diff_list(T1, T2, NewAcc);
diff_list([H1 | T1], [H2 | T2], Acc)
        when is_atom(H1), is_atom(H2);
             is_integer(H1), is_integer(H2);
             is_binary(H1), is_binary(H2);
             is_float(H1), is_integer(H2);
             is_float(H2), is_integer(H1);
             is_float(H1), is_float(H2)
        ->
    {D1, Tt2} = case without(H1, T2) of
        false -> {[{'$rm', H1}], T2};
        List2 -> {[], List2}
    end,
    {D2, Tt1} = case without(H2, T1) of
        false -> {[{'$add', H2}], T1};
        List1 -> {[], List1}
    end,
    diff_list(Tt1, Tt2, D2 ++ D1 ++ Acc);
%
% integer < atom < tuple < list < binary
diff_list([H1 | T1], [H2 | _ ] = L2, Acc)
        when H1 < H2 ->
    NewAcc = [{'$rm', H1} | Acc],
    diff_list(T1, L2, NewAcc);
diff_list([H1 | _] = L1, [H2 | T2], Acc)
        when H1 > H2 ->
    NewAcc = [{'$add', H2} | Acc],
    diff_list(L1, T2, NewAcc).


diff_tuple(One, One, Acc) ->
    list_to_tuple(lists:reverse(Acc) ++ One);
diff_tuple([H1 | T1], [H2 | T2], Acc) ->
    Diff = erlang_diff_ll(H1, H2),
    diff_tuple(T1, T2, [Diff | Acc]);
diff_tuple([], [H2 | T2], Acc) ->
    diff_tuple([], T2, [{'$add', H2} | Acc]);
diff_tuple([H1 | T1], [], Acc) ->
    diff_tuple(T1, [], [{'$remove', H1} | Acc]).

diff_tuple_in_list(T, T) ->
    [];
diff_tuple_in_list(T1, T2) ->
    [erlang_diff_ll(T1, T2)].


sort_everything(List) when is_list(List) ->
    case io_lib:printable_list(List) of
        true  -> List;
        false -> lists:sort([sort_everything(V) || V <- List])
    end;
sort_everything(Tuple) when is_tuple(Tuple) -> 
    TupleList = tuple_to_list(Tuple),
    TupleSorted = lists:map(fun(E) -> sort_everything(E) end, TupleList),
    list_to_tuple(TupleSorted);
sort_everything(Term) -> 
    Term.

tmpfile() ->
    _TmpFilename = os:cmd("echo -n `mktemp`").


without(Item, List) -> without(Item, List, []).
without(_Item, [], _) -> false;
without(Item, [Item|Tail], Acc) -> lists:reverse(Acc) ++ Tail;
without(Item, [Head|Tail], Acc) -> without(Item, Tail, [Head | Acc]).

take_closest_tuple(Item, List) ->
    Closest = lists:sort(lists:map(close_ratio(Item), List)),
    case Closest of
        [{Ratio, ListItem} | _] when Ratio < -4 ->
            {value, ListItem, without(ListItem, List)};
        _ ->
            false
    end.

close_ratio(Item) ->
    [H1 | T1] = tuple_to_list(Item),
    fun (ListItem) ->
            [H2 | T2] = tuple_to_list(ListItem),
            S1 = length(T1),
            S2 = length(T2),
            SMin = erlang:min(S1, S2),
            SizeMatch = bool_to_int(S1 == S2),
            Head = 5 * bool_to_int(H1 == H2),
            Tail = lists:sum(lists:zipwith(fun
                            (Same, Same) -> 1;
                            (_, _) -> 0
                        end, 
                        lists:sublist(T1, 1, SMin),
                        lists:sublist(T2, 1, SMin))),
            {-(SizeMatch + Head + Tail), ListItem}
    end.

bool_to_int(true) -> 1;
bool_to_int(_Any) -> 0.

