-module(rbt).

-export([add/2, contains/2, print_tree/1]).

% Balance Functions


-spec add(integer(), tuple() | nil) -> tuple().
add(Value, nil) ->
    {black, Value, nil, nil};
add(Value, Tree) ->
    {_, V, L, R} = add_r(Value, Tree),
    {black, V, L, R}. % Recolor the root node

% Add Recursive
-spec add_r(integer(), tuple()) -> tuple().
add_r(Value, nil) ->
    {red, Value, nil, nil};
add_r(Value, {C, Value, Left, R}) -> % Already in Tree
    {C, Value, Left, R};
add_r(Value, {C, NodeValue, Left, R}) when Value < NodeValue -> % Recur Left
    balance_left({C, NodeValue, add_r(Value, Left), R});
add_r(Value, {C, NodeValue, Left, R}) when Value > NodeValue -> % Recur Right
    balance_right({C, NodeValue, Left, add_r(Value, R)}).




-spec contains(integer(), tuple()) -> boolean().
contains(_, nil) -> false;
contains(Value, {_, Value, _, _}) -> true;
contains(Value, {_, NodeValue, Left, _}) when Value < NodeValue ->
    contains(Value, Left);
contains(Value, {_, NodeValue, _, R}) when Value > NodeValue ->
    contains(Value, R).



    
-spec balance_left(tuple()) -> tuple().
balance_left({black, V, {red, LV, LL, {red, LRV, LRL, LRR}}, {red, RV, RL, RR}}) -> % Push Redness
    {red, V, {black, LV, LL, {red, LRV, LRL, LRR}}, {black, RV, RL, RR}};
balance_left({black, V, {red, LV, {red, LLV, LLL, LLR}, LR}, {red, RV, RL, RR}}) -> % Push Redness
    {red, V, {black, LV, {red, LLV, LLL, LLR}, LR}, {black, RV, RL, RR}};
balance_left({black, V, {red, LV, {red, LLV, LLL, LLR}, LR}, R}) -> % Left Left Heavy
    % Single Rotate Right around Root
    % LR -> RL
    {black, LV, {red, LLV, LLL, LLR}, {red, V, LR, R}};
balance_left({black, V, {red, LV, LL, {red, LRV, LRL, LRR}}, R}) -> % Left Right Heavy
    % Single Rotate Left around Left, Single Rotate Right around Root
    % LRL -> LLR -> LR
    % LRR -> RL
    {black, LRV, {red, LV, LL, LRL}, {red, V, LRR, R}};
balance_left(Tuple) -> Tuple.


-spec balance_right(tuple()) -> tuple().
balance_right({black, V, {red, LV, LL, LR}, {red, RV, {red, RLV, RLL, RLR}, RR}}) -> % Push Redness
    {red, V, {black, LV, LL, LR}, {black, RV, {red, RLV, RLL, RLR}, RR}};
balance_right({black, V, {red, LV, LL, LR}, {red, RV, RL, {red, RRV, RRL, RRR}}}) -> % Push Redness
    {red, V, {black, LV, LL, LR}, {black, RV, RL, {red, RRV, RRL, RRR}}};
balance_right({black, V, L, {red, RV, RL, {red, RRV, RRL, RRR}}}) -> % Right Right Heavy
    % Single rotate left around root
    % RL -> LR
    {black, RV, {red, V, L, RL}, {red, RRV, RRL, RRR}};
balance_right({black, V, L, {red, RV, {red, RLV, RLL, RLR}, RR}}) -> % Right Left Heavy
    % Single Rotate right around right, Single rotate left around root
    % RLR -> RRL -> RL
    % RLL -> LR
    {black, RLV, {red, V, L, RLL}, {red, RV, RLR, RR}};
balance_right(Tuple) -> Tuple.


% Claude AI function to print a rbt
-spec print_tree(tuple() | nil) -> ok.
print_tree(Tree) ->
    io:format("~n"),
    print_tree_rec(Tree, 0),
    ok.
-spec print_tree_rec(tuple() | nil, integer()) -> ok.
print_tree_rec(nil, _) ->
    ok;
print_tree_rec({C, Value, Left, R}, Depth) ->
    print_tree_rec(R, Depth + 1),
    io:format("~s~s:~p~n", [string:copies("    ", Depth), C, Value]),
    print_tree_rec(Left, Depth + 1).


-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").

%% add/2 tests
add_test_() ->
    [ 
      %% Happy path - Adding to an empty tree
      ?_assertEqual({black, 10, nil, nil}, add(10, nil)),

      %% Happy path - Adding to a non-empty tree
      ?_assertEqual({black, 20, {red, 10, nil, nil}, nil}, 
                    add(10, {black, 20, nil, nil})),

      %% Edge Case - Adding a duplicate value (should retain original structure)
      ?_assertEqual({black, 10, {red, 5, nil, nil}, {red, 15, nil, nil}},
                    add(10, {black, 10, {red, 5, nil, nil}, {red, 15, nil, nil}})),
      
      %% Recoloring scenario: Adding 6 to a 3, 4, 5 tree
      % Before: {black, 4, {red, 3, nil, nil}, {red, 5, nil, nil}}
      % After: Recolor root to red, children to black: {black, 4, {black, 3, nil, nil}, {black, 5, nil, {red, 6, nil, nil}}}
      ?_assertEqual(
        {black, 4, {black, 3, nil, nil}, {black, 5, nil, {red, 6, nil, nil}}},
        add(6, {black, 4, {red, 3, nil, nil}, {red, 5, nil, nil}})
      ),

      %% Double rotation: Adding 8 to a 5, 10 tree
      % Before: {black, 5, nil, {red, 10, nil, nil}}
      % After: Rotate left around 5, recolor: {black, 8, {red, 5, nil, nil}, {red, 10, nil, nil}}
      ?_assertEqual(
          {black, 8, {red, 5, nil, nil}, {red, 10, nil, nil}},
          add(8, {black, 5, nil, {red, 10, nil, nil}})
      ),
  
      %% Adding 1 to a left-heavy tree (5, then 4, now adding 1)
      % Before: {black, 5, {red, 4, nil, nil}, nil}
      % After: Tree stays balanced with root as 4: {black, 4, {red, 1, nil, nil}, {red, 5, nil, nil}}
      ?_assertEqual(
          {black, 4, {red, 1, nil, nil}, {red, 5, nil, nil}},
          add(1, {black, 5, {red, 4, nil, nil}, nil})
      )
    
    ].

%% contains/2 tests
contains_test_() ->
    [ 
      %% Happy path - Value exists in the tree
      ?_assertEqual(true, contains(10, {black, 10, {red, 5, nil, nil}, nil})),

      %% Edge Case - Value does not exist in the tree
      ?_assertEqual(false, contains(25, {black, 10, {red, 5, nil, nil}, nil})),

      %% Edge Case - Checking empty tree
      ?_assertEqual(false, contains(5, nil)),

      %% Edge Case - Value is in the right subtree
      ?_assertEqual(true, contains(15, {black, 10, nil, {red, 15, nil, nil}}))
    ].

%% balance_left/1 tests
balance_left_test_() ->
    [
      %% Happy path - Push redness to children
      ?_assertEqual(
          {red, 20, {black, 10, nil, {red, 15, nil, nil}}, {black, 30, nil, nil}},
          balance_left({black, 20, {red, 10, nil, {red, 15, nil, nil}}, {red, 30, nil, nil}})
      ),

      %% Edge Case - Left-left heavy rotation
      ?_assertEqual(
          {black, 10, {red, 5, nil, nil}, {red, 20, nil, nil}},
          balance_left({black, 20, {red, 10, {red, 5, nil, nil}, nil}, nil})
      ),

      %% Edge Case - Left-right heavy rotation
      ?_assertEqual(
          {black, 15, {red, 10, nil, nil}, {red, 20, nil, nil}},
          balance_left({black, 20, {red, 10, nil, {red, 15, nil, nil}}, nil})
      )
    ].

%% balance_right/1 tests
balance_right_test_() ->
    [
      %% Happy path - Push redness to children
      ?_assertEqual(
          {red, 20, {black, 10, nil, nil}, {black, 30, {red, 25, nil, nil}, nil}},
          balance_right({black, 20, {red, 10, nil, nil}, {red, 30, {red, 25, nil, nil}, nil}})
      ),

      %% Edge Case - Right-right heavy rotation
      ?_assertEqual(
          {black, 30, {red, 20, nil, nil}, {red, 40, nil, nil}},
          balance_right({black, 20, nil, {red, 30, nil, {red, 40, nil, nil}}})
      ),

      %% Edge Case - Right-left heavy rotation
      ?_assertEqual(
          {black, 25, {red, 20, nil, nil}, {red, 30, nil, nil}},
          balance_right({black, 20, nil, {red, 30, {red, 25, nil, nil}, nil}})
      )
    ].

-endif.