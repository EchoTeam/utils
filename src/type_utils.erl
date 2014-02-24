%%% 
%%% Copyright (c) 2008-2014 JackNyfe, Inc. <info@jacknyfe.com>
%%% All rights reserved.
%%%
%%% vim: set ts=4 sts=4 sw=4 et:

-module(type_utils).

-export([
    str/1,
    to_binary/1,
    to_integer/1,
    to_list/1,
    to_float/1
]).

str(T = [X | _]) when is_integer(X) -> T; % pure string
str(T) when is_list(T) -> string:join(lists:map(fun str/1, T), "");
str(T) when is_atom(T) -> atom_to_list(T);
str(T) when is_binary(T) -> binary_to_list(T);
str(T) when is_integer(T) -> integer_to_list(T);
str(T) -> lists:flatten(io_lib:format("~p", [T])).

to_integer(I) when is_integer(I) -> I;
to_integer(I) when is_binary(I) -> to_integer(binary_to_list(I));
to_integer(I) when is_list(I) -> list_to_integer(I).

to_binary(X) when is_binary(X) -> X;
to_binary(X) when is_list(X) -> list_to_binary(X);
to_binary(X) when is_atom(X) -> list_to_binary(atom_to_list(X));
to_binary(X) when is_integer(X) -> list_to_binary(integer_to_list(X));
to_binary(X) -> term_to_binary(X).

to_list(Arg) when is_list(Arg) -> Arg;
to_list(Arg) when is_binary(Arg) -> binary_to_list(Arg);
to_list(Arg) when is_atom(Arg) -> atom_to_list(Arg);
to_list(Arg) when is_integer(Arg) -> integer_to_list(Arg);
to_list(Arg) when is_reference(Arg) -> erlang:ref_to_list(Arg).

to_float(X) when is_integer(X) -> float(X);
to_float(X) when is_binary(X) -> to_float(binary_to_list(X));
to_float(X) when is_list(X) ->
    case string:to_float(X) of
        {error, no_float} ->
            case string:to_integer(X) of
                {error, no_integer} -> 0;
                {Int, _} -> Int
            end;
        {Float,_} -> Float
    end;
to_float(X) when is_float(X) -> X;
to_float(_) -> 0.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

to_float_test_() ->
    [?_assertEqual(R, to_float(I)) || {R, I} <- [
        {10.4, "10.4"},
        {14.0, 14},
        {15.6, 15.6},
        {0, undefined},
        {10.4, <<"10.4">>}
    ]].

str_test_() ->
    [?_assertEqual(R, str(I)) || {R, I} <- [
        {"atom", atom},
        {"str", "str"},
        {"abcd101", ["ab", "cd", 101]},
        {"10.5", 10.5},
        {"test", <<"test">>},
        {"10", 10}
    ]].

-endif.
