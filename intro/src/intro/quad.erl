-module(quad).

-export([quad_form/3]).


-spec quad_form(integer(), integer(), integer()) -> {float(), float()} | error.
quad_form(A, B, C) when B * B < 4 * A * C -> 
    error;
quad_form(A, B, C) -> 
    D = math:sqrt(B * B - 4 * A * C),
    {
        (-B + D) / 2 / A,
        (-B - D) / 2 / A
    }.


