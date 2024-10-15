-module(rbt_old).

-export([single_rotate_left/1, single_rotate_right/1, push_redness/1]).

% Helper Functions

-spec single_rotate_right(tuple()) -> tuple().
single_rotate_right({C, V, {LC, LV, LL, LR}, R}) ->
    % Left Right disconnects and becomes Right Left
    {LC, LV, LL, {C, V, LR, R}}.

-spec single_rotate_left(tuple()) -> tuple().
single_rotate_left({C, V, L, {RC, RV, RL, RR}}) ->
    % Right Left disconnects and becomes Left Right 
    {RC, RV, {C, V, L, RL}, RR}.

-spec push_redness(tuple()) -> tuple().
push_redness({black, V, {red, LV, LL, LR}, {red, RV, RL, RR}}) -> 
    {red, V, {black, LV, LL, LR}, {black, RV, RL, RR}};
push_redness(Tuple) -> Tuple.
