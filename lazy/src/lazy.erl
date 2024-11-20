-module(lazy).

-export([get_today/0, get_next_day/1, start_rnd_stream/1, start/0, start_date_stream/1, next/1, recieve_date/1, stop/1, get_lcg_rnd/1, random_sequence/2]).



get_today() -> 
    {Date, _} = calendar:local_time(),
    Date.

get_lcg_rnd(Seed) -> 
    math:fmod((Seed * 1664525 + 1013904223), math:pow(2, 32)).


random_sequence(Seed, Count) when Count > 0 ->
    [get_lcg_rnd(Seed) | random_sequence(get_lcg_rnd(Seed) + 2, Count - 1)];
random_sequence(_, 0) ->
        [].

% Get the next day of the given day
get_next_day(Date) ->
    DateBefore = calendar:date_to_gregorian_days(Date),
    calendar:gregorian_days_to_date(DateBefore + 1).






% Start with today's date
start() ->
    {Date, _} = calendar:local_time(),
    {start_date_stream(Date), start_rnd_stream(1234)}.

% Start with a specific date
start_date_stream(InitialDate) ->
    spawn(fun() -> recieve_date(InitialDate) end).


start_rnd_stream(Seed) -> 
    spawn(fun() -> recieve_rnd(Seed) end).


next(Pid) ->
    Pid ! {next, self()},
    receive
        {date, Date} -> Date;
        {rnd, Rng} -> Rng
    end.

% Stop the stream
stop(Pid) ->
    Pid ! stop.

% Internal loop functions
recieve_date(CurrentDate) ->
    receive
        {next, From} ->
            NextDate = get_next_day(CurrentDate),
            From ! {date, NextDate},
            recieve_date(NextDate);
        stop ->
            ok
    end.

recieve_rnd(Seed) -> 
    receive
        {next, From} ->
            NextRng = get_lcg_rnd(Seed + 2),
            From ! {rnd, NextRng},
            recieve_rnd(NextRng);
        stop ->
            ok
    end.



% RANDOM NUMBER GENERATOR LCG Linear COngruential Generator









-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").

date_stream_test_() ->
    [
        ?_assertEqual({2024, 11, 19}, get_next_day({2024, 11, 18})),
        ?_assertEqual({2024, 2, 29}, get_next_day({2024, 2, 28})),
        ?_assertEqual({2025, 3, 1}, get_next_day({2025, 2, 28}))
    ].





-endif.