-module(actor).

-export([start/0, stop/1, calc_factorial/1]).


start() ->
    Factorial_Pid = spawn(fun() -> factorial_process(0) end),
    {Factorial_Pid, spawn(fun() -> combo(0, Factorial_Pid) end)}.

factorial_process(Calculations) -> 
    receive
      {calc_factorial, Int, From} ->
        Result = calc_factorial(Int),
        From ! {factorial_result, Result},
        factorial_process(Calculations + 1);
      stop -> 
        io:format("Stopping factorial process~n"),
        ok
    end.

combo(Calculations, Factorial_Pid) ->
    receive
      {calc_permutation, N, R, From} ->
        Factorial_Pid ! {calc_factorial, N, self()},
        Numerator = receive
            {factorial_result, Result} -> Result
        end,
        Factorial_Pid ! {calc_factorial, N - R, self()},
        Denominator = receive
            {factorial_result, Result2} -> Result2
        end,
        From ! {permutation, Numerator div Denominator},
        combo(Calculations + 1, Factorial_Pid);
    %   {combination, N, R, From} ->
    %     From ! answer,
    %     combo(Calculations + 1, Factorial_Pid);
      stop ->
        io:format("Stopping combo process~n"),
        ok;
      _ -> 
        error
    end.

stop(Pid) ->
    Pid ! stop.


calc_factorial(2) ->
    2;
calc_factorial(Int) ->
    Int * calc_factorial(Int - 1).



-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").



-endif.