-module(lazy).

-export([get_today/0, get_next_day/1, send_message/3, start/0, start/1, next/1, loop/1, stop/1]).



send_message(Pid,Message,Response_handler)->
    Pid ! {self(),Message},
    receive
        Response -> Response_handler(Response)
    end.


get_today() -> 
    {Date, _} = calendar:local_time(),
    Date.

% Get the next day of the given day
get_next_day(Date) ->
    DateBefore = calendar:date_to_gregorian_days(Date),
    calendar:gregorian_days_to_date(DateBefore + 1).






% Start with today's date
start() ->
    {Date, _} = calendar:local_time(),
    start(Date).

% Start with a specific date
start(InitialDate) ->
    spawn(fun() -> loop(InitialDate) end).

next(Pid) ->
    Pid ! {next, self()},
    receive
        {date, Date} -> Date
    end.

% Stop the stream
stop(Pid) ->
    Pid ! stop.

% Internal loop function
loop(CurrentDate) ->
    receive
        {next, From} ->
            NextDate = get_next_day(CurrentDate),
            From ! {date, NextDate},
            loop(NextDate);
        stop ->
            ok
    end.





-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").

date_stream_test_() ->
    ?_test(
        setup(
            fun setup_stream/0,      % Setup function: initializes the stream
            fun teardown_stream/1,   % Teardown function: stops the stream
            fun test_logic/1         % Test logic: uses the stream
        )
    ).

setup_stream() ->
    lazy:start({2024, 11, 18}).  % Start the stream and return the Pid

teardown_stream(Stream) ->
    lazy:stop(Stream).  % Stop the stream process

test_logic_(Stream) ->
    [
        ?_assertEqual({2024, 11, 19}, lazy:next(Stream)),
        ?_assertEqual({2024, 11, 20}, lazy:next(Stream)),
        ?_assertEqual({2024, 11, 21}, lazy:next(Stream))
    ].








-endif.