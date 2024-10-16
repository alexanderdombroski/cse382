-module(heap).

-export([insert/2, destroy_min/1, get_min/1]).

% { path_to_nil, value, left, right }
-type heap_node() :: {integer(), integer(), heap_node() | nil, heap_node() | nil}.


-spec insert(integer(), heap_node()) -> heap_node().
insert(V, nil) -> 
    {1, V, nil, nil}; % Insert Node
insert(V, Heap) -> 
    merge({1, V, nil, nil}, Heap).


-spec get_min(heap_node() | nil) -> integer().
get_min(nil) -> nil;
get_min({_, V, _, _}) -> V.


-spec destroy_min(heap_node()) -> heap_node().
destroy_min(nil) -> nil;
destroy_min({_, _, L, R}) -> merge(L, R).



-spec merge(heap_node(), heap_node()) -> heap_node().
merge(L, nil) -> L;
merge(nil, R) -> R;
merge({_, V1, L1, R1}, {Rank2, V2, L2, R2}) when V1 =< V2 ->
    build_node(V1, L1, merge(R1, {Rank2, V2, L2, R2})); 
merge({Rank1, V1, L1, R1}, {_, V2, L2, R2}) ->
    build_node(V2, L2, merge({Rank1, V1, L1, R1}, R2)).


-spec build_node(integer(), heap_node(), heap_node()) -> heap_node().
build_node(V, L, R) -> 
    case {rank(L), rank(R)} of
        {A, B} when A >= B ->
            {B+1, V, L, R};
        {A, _} ->
            {A+1, V, L, R}
    end.

-spec rank(heap_node() | nil) -> integer().
rank(nil) -> 0;
rank({R, _, _, _}) -> R.


-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").


%% Tests for insert/2
insert_test_() ->
    [
        %% Happy path - Inserting into an empty heap
        ?_assertEqual({1, 10, nil, nil}, heap:insert(10, nil)),

        %% Inserting a smaller value (10) into an existing heap with root 20
        %% Expected: 10 becomes the new root
        ?_assertEqual(
            {1, 10, nil, {1, 20, nil, nil}},
            heap:insert(10, {1, 20, nil, nil})
        ),

        %% Inserting a larger value (30) into an existing heap with root 20
        %% Expected: 20 remains the root, with 30 in the right subtree
        ?_assertEqual(
            {1, 20, nil, {1, 30, nil, nil}},
            heap:insert(30, {1, 20, nil, nil})
        )
    ].

%% Tests for get_min/1
get_min_test_() ->
    [
        %% Happy path - Retrieving the minimum element (root value)
        ?_assertEqual(5, heap:get_min({1, 5, nil, nil})),

        %% Edge case - Retrieving from an empty heap
        ?_assertEqual(nil, heap:get_min(nil)),

        %% Retrieving the minimum element from a larger heap
        ?_assertEqual(10, heap:get_min({1, 10, {1, 15, nil, nil}, {1, 20, nil, nil}}))
    ].

%% Tests for destroy_min/1
destroy_min_test_() ->
    [
        %% Happy path - Destroying the minimum (root) element from a heap with one element
        ?_assertEqual(nil, heap:destroy_min({1, 10, nil, nil})),

        %% Destroying the root of a heap with two elements
        %% Expected: Merge the children, result should have only the remaining node
        ?_assertEqual({1, 20, nil, nil}, heap:destroy_min({1, 10, nil, {1, 20, nil, nil}})),

        %% Destroying the root of a larger heap
        %% Expected: Heap will merge the two subtrees
        ?_assertEqual(
            {1, 15, nil, {1, 20, nil, nil}},
            heap:destroy_min({1, 10, {1, 15, nil, nil}, {1, 20, nil, nil}})
        ),

        %% Edge case - Destroying the root from an empty heap
        ?_assertEqual(nil, heap:destroy_min(nil))
    ].

-endif.

