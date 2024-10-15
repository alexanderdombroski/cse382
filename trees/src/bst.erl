-module(bst).

-export([add/2, contains/2]).


-spec add(integer(), tuple()) -> tuple().
add(Value, {Value, Left, Right}) -> % Value already in Tree
    {Value, Left, Right};
add(Value, {NodeValue, Left, Right}) when Value < NodeValue ->
    {NodeValue, add(Value, Left), Right}; 
add(Value, {NodeValue, Left, Right}) when Value > NodeValue ->
    {NodeValue, Left, add(Value, Right)};
add(Value, nil) -> % Add to empty Node
    {Value, nil, nil}.


-spec contains(integer(), tuple()) -> boolean().
contains(_, nil) -> false;
contains(Value, {Value, _, _}) -> true;
contains(Value, {NodeValue, Left, _}) when Value < NodeValue ->
    contains(Value, Left);
contains(Value, {NodeValue, _, Right}) when Value > NodeValue ->
    contains(Value, Right).




    
-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").

add_test_() ->
    [   
        %% Happy path: Add a value to a non-empty tree
        ?_assertEqual({10, {5, nil, nil}, nil}, add(5, {10, nil, nil})),
        ?_assertEqual({10, {5, nil, nil}, {15, nil, nil}}, add(15, {10, {5, nil, nil}, nil})),
        
        %% Happy path: Add a value to a more complex tree
        ?_assertEqual({10, {5, {3, nil, nil}, nil}, {15, nil, nil}}, add(3, {10, {5, nil, nil}, {15, nil, nil}})),
        
        %% Edge case: Add a value to an empty tree
        ?_assertEqual({5, nil, nil}, add(5, nil)),
        
        %% Edge case: Add a duplicate value (assuming the original value remains unchanged)
        ?_assertEqual({10, {5, nil, nil}, nil}, add(5, {10, {5, nil, nil}, nil})),
        
        %% Edge case: Add a value that goes deeper into the right subtree
        ?_assertEqual({10, {5, nil, nil}, {15, nil, {20, nil, nil}}}, add(20, {10, {5, nil, nil}, {15, nil, nil}}))
    ].

contains_test_() ->
    [
        %% Happy path: Value exists in the tree
        ?_assertEqual(true, contains(5, {10, {5, nil, nil}, {15, nil, nil}})),
        ?_assertEqual(true, contains(15, {10, {5, nil, nil}, {15, nil, nil}})),
        
        %% Happy path: Check root node
        ?_assertEqual(true, contains(10, {10, {5, nil, nil}, {15, nil, nil}})),

        %% Edge case: Value does not exist in the tree
        ?_assertEqual(false, contains(7, {10, {5, nil, nil}, {15, nil, nil}})),

        %% Edge case: Search in an empty tree
        ?_assertEqual(false, contains(10, nil)),

        %% Edge case: Search for a value deeper in the tree
        ?_assertEqual(true, contains(3, {10, {5, {3, nil, nil}, nil}, {15, nil, nil}})),
        ?_assertEqual(false, contains(9, {10, {5, {3, nil, nil}, nil}, {15, nil, nil}}))
    ].

-endif.