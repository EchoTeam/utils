%%% 
%%% Copyright (c) 2008-2014 JackNyfe, Inc. <info@jacknyfe.com>
%%% All rights reserved.
%%%
%%% vim: set ts=4 sts=4 sw=4 et:

-module(list_utils).

-export([
    chunk_by/2,
    first_ne/2,
    empty/1,
    group/1,
    group_by/2,
    group_by_key/2,
    get_proplist_values/3,
    ignore_empty/1,
    replace_proplist_values/2,
    stable_unique_list/1,
    unique_list/1
]).

% The first element of the list wich is not equal to the value specified.
% If no such element found, specified value returned.
first_ne( X, []) -> X;
first_ne( X, [X | Rest]) -> first_ne(X, Rest);
first_ne(_X, [Y | _]) -> Y.

empty(Value) ->
    lists:member(Value, ["", <<>>, undefined, null]).

% Group the list into a list of lists of equal, adjacent elements.
group(List) -> group_by(List, [], fun(A, B) -> A =:= B end).
group_by_key(N, List) -> group_by(List, [],
    fun(A, B) -> element(N, A) =:= element(N, B) end).
group_by(Pred, List) -> group_by(List, [], Pred).
group_by([], [], _Pred) -> [];
group_by([], Cur, _Pred) -> [lists:reverse(Cur)];
group_by([H|Rest], [], Pred) -> group_by(Rest, [H], Pred);
group_by([H|Rest], Cur, Pred) ->
    case Pred(hd(Cur), H) of
        true -> group_by(Rest, [H|Cur], Pred);
        false -> [lists:reverse(Cur) | group_by(Rest, [H], Pred)]
    end.

get_proplist_values(Keys, PL, Default) ->
    [proplists:get_value(K, PL, Default) || K <- Keys].

ignore_empty(List) ->
    [{K,V} || {K,V} <- List, V /= [], V /= undefined, V /= <<>>].

replace_proplist_values(Values, PL) ->
    lists:foldl(
        fun ({K,V}, Acc) -> lists:keystore(K, 1, Acc, {K,V});
            (_, Acc) -> Acc
        end, PL, Values).

stable_unique_list(List) ->
    {UniqueList, _} = lists:foldl(fun(E, {Acc, Dict}) ->
        case dict:find(E, Dict) of
            error -> {[E | Acc], dict:store(E, true, Dict)};
            _ -> {Acc, Dict}
        end
    end, {[], dict:new()}, List),
    lists:reverse(UniqueList).

unique_list(List) ->
    ordsets:to_list(ordsets:from_list(List)).

% Cut potentially long list into chunks of size N.
chunk_by(N, List) when N > 0 ->
    try lists:split(N, List) of
        {FirstN, Rest} -> [FirstN | chunk_by(N, Rest)]
    catch
        error:badarg when List /= [] -> [List];
        error:badarg -> []
    end.
