%%% 
%%% Copyright (c) 2008-2014 JackNyfe, Inc. <info@jacknyfe.com>
%%% All rights reserved.
%%%
%%% vim: set ts=4 sts=4 sw=4 et:

-module(time_utils).

-export([
    ago_of_ts/1,
    date_of_ts/1,
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

date_of_ts({_,_,_} = Now) -> date_of_ts(time_utils:unixtime(Now));
date_of_ts(TS) ->
    DateTime = calendar:gregorian_seconds_to_datetime(62167219200+TS),
    {{Year, Month, Day}, {Hour, Min, Sec}} = DateTime,
    io_lib:format("~4..0B-~2..0B-~2..0B ~2B:~2.10.0B:~2.10.0B",
        [Year, Month, Day, Hour, Min, Sec]).

ago_of_ts({_,_,_} = Now) ->
    ago_of_ts(unixtime(Now));
ago_of_ts(TS) ->
    Diff = unixtime() - TS,
    Formats = [
        {60, "seconds", 1},
        {90, "1 minute"},
        {3600, "minutes", 60},
        {5400, "1 hour"},
        {86400, "hours", 3600},
        {129600, "1 day"},
        {604800, "days", 86400},
        {907200, "1 week"},
        {2628000, "weeks", 604800},
        {3942000, "1 month"},
        {31536000, "months", 2628000},
        {47304000, "1 year"},
        {3153600000, "years", 31536000}
    ],
    lists:foldl(fun(F, Acc) ->
        case Diff < element(1, F) of
            true -> (case tuple_size(F) of
                2 -> element(2, F);
                3 -> integer_to_list(erlang:round(Diff/element(3, F)))
                                        ++ " " ++ element(2, F) end) ++ " ago";
            false -> Acc
        end
    end, "Just now", lists:reverse(Formats)).

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

