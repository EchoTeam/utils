%%%
%%% Copyright (c) 2013 JackNyfe. All rights reserved.
%%% THIS SOFTWARE IS PROPRIETARY AND CONFIDENTIAL. DO NOT REDISTRIBUTE.
%%%
%%% vim: set ts=4 sts=4 sw=4 et:

-module(time_utils).

-export([
    iso8601_fmt/1,
    iso8601_fmt/2,
    micro2now/1,
    now2micro/1,
    now2ms/1,
    unixtime/0,
    unixtime/1,
    unixtime_float/0,
    unixtime_float/1
]).

iso8601_fmt(DateTime) ->
    iso8601_fmt(DateTime, "Z").

iso8601_fmt(DateTime, Zone) ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = DateTime,
    lists:flatten(
        io_lib:format("~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0B~s",
            [Year, Month, Day, Hour, Min, Sec, Zone])
    ).

micro2now(T) ->
    Res = lists:foldl(fun(D, {R, L}) ->
            V = R div D,
            {R-V*D, [V|L]}
    end, {T,[]}, [1000000000000, 1000000]),
    list_to_tuple(lists:reverse(lists:flatten(tuple_to_list(Res)))).

now2micro({Mega, Sec, Micro}) -> Mega * 1000000000000 + Sec * 1000000 + Micro.

now2ms(Now) -> now2micro(Now) div 1000.

% Time stamp of current time.
%% @spec unixtime() -> integer()
unixtime() -> unixtime(now()).

%% @spec unixtime(now()) -> integer()
unixtime({Mega, Secs, _Msecs}) -> Mega * 1000000 + Secs.

%% @spec unixtime_float() -> float()
unixtime_float() -> unixtime_float(now()).

%% @spec unixtime_float(now()) -> float()
unixtime_float({M,S,U}) -> M*1000000 + S + U/1000000.

