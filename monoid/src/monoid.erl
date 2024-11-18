-module(monoid).

-export([sum_list/1, sum/2, invert_color/1, to_sepia/1, blend_colors/3, brighten_color/2]).

% -------------------- Types --------------------


-type color() :: {integer(), integer(), integer()} | integer().

% -------------------- Monoid Functions --------------------

% Addition with positive integers
% Subtraction with integers
% Multiplication with positive integers
% concatonation with strings

-spec sum_list(list(integer())) -> integer().
sum_list([H | T]) -> 
    H + sum_list(T).

-spec sum(integer(), integer()) -> integer().
sum(X, Y) -> X + Y.

-spec invert_color(color()) -> color().
invert_color({R, G, B}) -> 
    {255-R, 255-G, 255-B};
invert_color(Color) when is_integer(Color) ->
    invert_color(from_hex(Color)).

% Factor is 1.2 for increasing brightness of a color by 20%
-spec brighten_color(color(), float()) -> color().
brighten_color({R, G, B}, Factor) ->
    {
        min(round(R * Factor), 255),
        min(round(G * Factor), 255),
        min(round(B * Factor), 255)
    };
brighten_color(Color, Factor) when is_integer(Color) -> 
    brighten_color(from_hex(Color), Factor).

to_sepia({R, G, B}) ->
    {
        min(round(0.393 * R + 0.769 * G + 0.189 * B), 255),
        min(round(0.349 * R + 0.686 * G + 0.168 * B), 255),
        min(round(0.272 * R + 0.534 * G + 0.131 * B), 255)
    };
to_sepia(Color) when is_integer(Color) -> 
    to_sepia(from_hex(Color)).

blend_colors({R1, G1, B1}, {R2, G2, B2}, Factor) ->
    {
        round(R1 * (1 - Factor) + R2 * Factor),
        round(G1 * (1 - Factor) + G2 * Factor),
        round(B1 * (1 - Factor) + B2 * Factor)
    };
blend_colors(Color1, Color2, Factor) when is_integer(Color1) + is_integer(Color2) ->
    blend_colors(from_hex(Color1), from_hex(Color2), Factor).


from_hex(Color) -> 
    {
        (Color bsr 16) band 16#FF,
        (Color bsr 8) band 16#FF,
        Color band 16#FF
    }.






% -------------------- Tests --------------------

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").

% Tests for sum_list/1
sum_list_test_() ->
    [
        %% Happy Path
        {"sum of positive integers", 
            fun() -> ?_assertEqual(10, my_module:sum_list([1, 2, 3, 4])) end},
        %% Edge Cases
        {"empty list", 
            fun() -> ?_assertEqual(0, my_module:sum_list([])) end},
        {"large integers", 
            fun() -> ?_assertEqual(5000000000, my_module:sum_list([1000000000, 2000000000, 2000000000])) end},
        {"negative integers", 
            fun() -> ?_assertEqual(-10, my_module:sum_list([-5, -3, -2])) end}
    ].

% Tests for sum/2
sum_test_() ->
    [
        %% Happy Path
        {"sum of two positive numbers", 
            fun() -> ?_assertEqual(5, my_module:sum(2, 3)) end},
        %% Edge Cases
        {"sum of zero and a number", 
            fun() -> ?_assertEqual(3, my_module:sum(3, 0)) end},
        {"sum with negative numbers", 
            fun() -> ?_assertEqual(-1, my_module:sum(3, -4)) end},
        {"sum of two large numbers", 
            fun() -> ?_assertEqual(2000000000, my_module:sum(1000000000, 1000000000)) end}
    ].

% Tests for invert_color/1
invert_color_test_() ->
    [
        %% Happy Path
        {"invert RGB tuple", 
            fun() -> ?_assertEqual({0, 255, 128}, my_module:invert_color({255, 0, 127})) end},
        %% Edge Cases
        {"invert black", 
            fun() -> ?_assertEqual({255, 255, 255}, my_module:invert_color({0, 0, 0})) end},
        {"invert white", 
            fun() -> ?_assertEqual({0, 0, 0}, my_module:invert_color({255, 255, 255})) end},
        {"invert mid-gray", 
            fun() -> ?_assertEqual({128, 128, 128}, my_module:invert_color({127, 127, 127})) end},
        {"invert hex integer color", 
            fun() -> ?_assertEqual({0, 255, 128}, my_module:invert_color(16#FF007F)) end}
    ].



-endif.
