%%%
%%% Copyright (c) 2013 JackNyfe. All rights reserved.
%%% THIS SOFTWARE IS PROPRIETARY AND CONFIDENTIAL. DO NOT REDISTRIBUTE.
%%%
%%% vim: set ts=4 sts=4 sw=4 et:

-module(string_utils).

-export([
    charlist_to_utf8/1,
    hex/1,
    hex/2,
    intersperse/2,
    is_ascii/1,
    is_utf8/1,
    md5_str/1,
    subst/3,
    unhtmlize/1,
    utf8_split/2,
    utf8_to_charlist/1
]).

charlist_to_utf8(List) ->
    unicode:characters_to_binary(List).

hex(B) -> hex(B, []).

hex(<<>>, Accum) ->
    lists:reverse(Accum);
hex(<<A:4/integer,B:4/integer,Rest/binary>>, Accum) ->
    hex(Rest, [dig(B), dig(A) | Accum]).

% string:join(What, With) ?
intersperse(With, What) -> lists:reverse(intersperse(What, With, [])).
intersperse([A,B|T], With, Acc) -> intersperse([B|T], With, [With, A | Acc]);
intersperse([A], _With, Acc) -> [A|Acc];
intersperse([], _With, Acc) -> Acc.

is_ascii(Str) when is_list(Str) ->
    lists:all(fun(C) -> xmerl_ucs:is_ascii(C) end, Str);
is_ascii(C) when is_integer(C) ->
    xmerl_ucs:is_ascii(C).

is_utf8(Str) ->
    case utf8_to_charlist(type_utils:to_binary(Str)) of
        {_, _, _} -> false;
        _ -> true
    end.

md5_str(Data) -> hex(erlang:md5(Data)).

subst(String, Search, Replacement) ->
    LStr = string:len(String),
    LSearch = string:len(Search),
    case string:str(String, Search) of
        0 -> String;
        Pos ->
            Left = string:left(String, Pos - 1),
            Right = string:right(String, LStr - LSearch - Pos + 1),
            lists:flatten([Left, Replacement, Right])
    end.

unhtmlize(String) -> unhtmlize(String, []).
unhtmlize([], Acc) -> lists:reverse(Acc);
unhtmlize([$&, $a, $m, $p, $; | T], Acc) ->
    unhtmlize(T, [$& | Acc]);
unhtmlize([$&, $l, $t, $; | T], Acc) ->
    unhtmlize(T, [$< | Acc]);
unhtmlize([$&, $g, $t, $; | T], Acc) ->
    unhtmlize(T, [$> | Acc]);
unhtmlize([$&, $q, $u, $o, $t, $; | T], Acc) ->
    unhtmlize(T, [$" | Acc]);
unhtmlize([Char | T], Acc) ->
    unhtmlize(T, [Char | Acc]).

utf8_split(Position, List) ->
    <<Prefix:Position/binary, Suffix/binary>> = charlist_to_utf8(List),
    {utf8_to_charlist(Prefix), utf8_to_charlist(Suffix)}.

utf8_to_charlist(Binary) ->
    unicode:characters_to_list(Binary).

%%% Internal functions

dig(N) when N < 10 -> $0 + N;
dig(N) -> $W + N.
