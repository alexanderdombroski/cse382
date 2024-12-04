-module(actor).

-export([start/0, stop/1, main/0]).


start() ->
    Factorial_Pid = spawn(fun() -> factorial_process(0) end),
    {Factorial_Pid, spawn(fun() -> combo_process(0, Factorial_Pid) end)}.

factorial_process(Calculations) -> 
    receive
      {calc_factorial_r, Int, From} ->
        From ! {factorial_result, calc_factorial_r(Int)},
        factorial_process(Calculations + 1);
      {get_calculations, From} ->
        From ! {calculations, Calculations},
        factorial_process(Calculations);
      stop -> 
        io:format("Stopping factorial process~n"),
        ok;
      _ ->
        error
    end.

combo_process(Calculations, Factorial_Pid) ->
    receive
      {calc_permutation, N, R, From} ->
        % io:format("permutaion message recieved~n"),
        Factorial_Pid ! {calc_factorial_r, N, self()},
        Numerator = receive
            {factorial_result, Result} -> Result
        end,
        Factorial_Pid ! {calc_factorial_r, N - R, self()},
        Denominator = receive
            {factorial_result, Result2} -> Result2
        end,
        From ! {permutation, Numerator div Denominator},
        combo_process(Calculations + 1, Factorial_Pid);
      {calc_combination, N, R, From} ->
        % io:format("Combination message recieved~n"),
        Factorial_Pid ! {calc_factorial_r, N, self()},
        Numerator = receive
            {factorial_result, Result} -> Result
        end,
        Factorial_Pid ! {calc_factorial_r, R, self()},
        D1 = receive
            {factorial_result, Result2} -> Result2
        end,
        Factorial_Pid ! {calc_factorial_r, N - R, self()},
        D2 = receive
            {factorial_result, Result3} -> Result3
        end,
        From ! {combination, (Numerator div D1) div D2},
        combo_process(Calculations + 1, Factorial_Pid);
      {compare_calculations, From} ->
        Factorial_Pid ! {get_calculations, self()},
        Factorial_Calculations = receive
            {calculations, Result} -> Result
        end,
        From ! {calculations, {Calculations, Factorial_Calculations}},
        combo_process(Calculations, Factorial_Pid);
      stop ->
        io:format("Stopping combo_process process~n"),
        ok;
      _ -> 
        error
    end.

stop(Pid) ->
    Pid ! stop.


calc_factorial_r(0) -> 1;
calc_factorial_r(1) -> 1;
calc_factorial_r(2) -> 2;
calc_factorial_r(Int) ->
    Int * calc_factorial_r(Int - 1).



calc_permutation(Cpid, N, R) ->
  Cpid ! {calc_permutation, N, R, self()},
  receive
    {permutation, Result} ->
      io:format("~p nPr ~p = ~p~n", [N, R, Result]);
    error -> error
  end.



calc_combination(Cpid, N, R) ->
  Cpid ! {calc_combination, N, R, self()},
  receive
    {combination, Result} ->
      io:format("~p nCr ~p = ~p~n", [N, R, Result]);
    error -> error
  end.

get_calc_numbers(Cpid) ->
  Cpid ! {compare_calculations, self()},
  receive
    {calculations, {Combination_Calculations, Factorial_Calculations}} ->
      io:format("Combination_Calculations: ~p~n", [Combination_Calculations]),
      io:format("Factorial_Calculations: ~p~n", [Factorial_Calculations])
  end,
  ok.


main() ->
  {_, Cpid} = start(),

  io:format("Processes Started~n"),
  
  calc_combination(Cpid, 4, 1),
  calc_combination(Cpid, 4, 2),
  calc_combination(Cpid, 7, 3),
  
  get_calc_numbers(Cpid),
  io:format("Combinations Calculated~n~n"),
  
  calc_permutation(Cpid, 4, 1),
  calc_permutation(Cpid, 4, 2),
  calc_permutation(Cpid, 7, 3),
  
  get_calc_numbers(Cpid),
  io:format("Permutations Calculated~n~n"),
  ok.





-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").



-endif.