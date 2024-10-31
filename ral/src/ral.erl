-module(ral).

-export([get/2, update/3, add/2, to_bits/2]).

-type random_access_list() :: list(node()).
-type ral_node() :: {integer(), integer() | nil, ral_node() | nil, ral_node() | nil}.


% Takes an index and a random access list and returns the value at that index
-spec get(integer(), random_access_list()) -> integer().
get(_, []) -> nil; % End of RAL
get(I, [{C, _, _, _} | T]) when I > C -> % Iter to the correct Tree
    get(I - C, T);
get(I, [{C, V, L, R} | _]) ->
    find_using_traversal_list({C, V, L, R}, to_bits(C - I, C)).


% Takes a value, an index, and a RAL
-spec update(integer(), integer(), random_access_list()) -> random_access_list().
update(_, _, []) -> []; % End of RAL
update(U, I, [{C, V, L, R} | T]) when I > C -> 
    [{C, V, L, R} | update(U, I - C, T)]; % Next Tree
update(U, I, [{C, V, L, R} | T]) -> 
    [replace_using_traversal_list(U, {C, V, L, R}, to_bits(C - I, C)) | T]. % Replace Head Tree and keep rest

-spec to_bits(integer(), integer()) -> list(binary()).
to_bits(Int, Max) -> <<Int:(trunc(math:log2(Max)))/unit:1>>.

% Takes a Binary Tree and a bitlist to find a node
-spec find_using_traversal_list(ral_node(), binary()) -> integer().
find_using_traversal_list({_, V, _, _}, <<>>) -> V;
find_using_traversal_list({_, _, L, _}, <<1:1, T/bitstring>>) -> 
    find_using_traversal_list(L, T);
find_using_traversal_list({_, _, _, R}, <<0:1, T/bitstring>>) -> 
    find_using_traversal_list(R, T).

% Takes an update number, a node
-spec replace_using_traversal_list(integer(), ral_node(), binary()) -> ral_node().
replace_using_traversal_list(V, _, <<>>) -> 
    {1, V, nil, nil};
replace_using_traversal_list(V, {C, _, L, R}, <<1:1, T/bitstring>>) -> 
    {C, nil, replace_using_traversal_list(V, L, T), R};
replace_using_traversal_list(V, {C, _, L, R}, <<0:1, T/bitstring>>) -> 
    {C, nil, L, replace_using_traversal_list(V, R, T)}.

% Adds item to the the ral
-spec add(integer(), random_access_list()) -> ok.
add(V, nil) -> 
    [{1, V, nil, nil}];
add(V, []) -> 
    [{1, V, nil, nil}];
add(V, [{C, NV, L, R} | T]) when C == 1 -> 
    [merge({1, V, nil, nil}, [{C, NV, L, R} | T])]; 
add(V, Ral) ->
    [{1, V, nil, nil} | Ral].


% Helper function for keeping the structure when adding values
-spec merge(ral_node(), ral_node()) -> ral_node().
merge(Node, []) -> Node;
merge({C, LV, LL, LR}, [{C, RV, RL, RR} | T]) -> 
    merge({C + C, nil, {C, LV, LL, LR}, {C, RV, RL, RR}}, T);
merge(Node, [H | T]) -> 
    [Node | merge(H, T)].

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").

% Test for get/2 function
get_test_() ->
    Ral = [{1, 10, nil, nil}, {2, nil, {1, 20, nil, nil}, {1, 30, nil, nil}}],
    [?_assertEqual(30, get(3, Ral)),  % Happy path: Correct index
     ?_assertEqual(20, get(2, Ral)),  % Happy path: Another valid index
     ?_assertEqual(10, get(1, Ral)),  % Happy path: Another valid index
     ?_assertEqual(nil, get(15, Ral)), % Edge case: Index out of bounds
     ?_assertEqual(nil, get(0, []))   % Edge case: Empty RAL
    ].

% Test for update/3 function
update_test_() ->
    Ral = [{1, 12, nil, nil}, {2, nil, {1, 20, nil, nil}, {1, 30, nil, nil}}],
    [?_assertEqual([{1, 99, nil, nil}, {2, nil, {1, 20, nil, nil}, {1, 30, nil, nil}}], 
                    update(99, 1, Ral)),  % Happy path: Update first element
     ?_assertEqual([{1, 12, nil, nil}, {2, nil, {1, 20, nil, nil}, {1, 99, nil, nil}}], 
                    update(99, 3, Ral)),  % Happy path: Update second element
     ?_assertEqual([{1, 12, nil, nil}, {2, nil, {1, 20, nil, nil}, {1, 30, nil, nil}}], 
                    update(99, 15, Ral)), % Edge case: Index out of bounds (return unchanged)
     ?_assertEqual([], update(99, 1, [])) % Edge case: Empty RAL
    ].

% Test for add/2 function
add_test_() ->
    [?_assertEqual([{1, 5, nil, nil}], add(5, [])), % Happy path: Add to empty RAL
     ?_assertEqual([{2, nil, {1, 5, nil, nil}, {1, 10, nil, nil}}], 
                   add(5, [{1, 10, nil, nil}])), % Happy path: Add and merge nodes
     ?_assertEqual([{2, nil, {1, 5, nil, nil}, {1, 10, nil, nil}}], 
                   add(5, [{1, 10, nil, nil}])), % Happy path: Add to non-empty RAL
     ?_assertEqual([{1, 5, nil, nil}], add(5, nil)) % Edge case: Adding to nil
    ].

merge_test() ->
    Leaf1 = {1, 6, nil, nil},
    Leaf2 = {1, 3, nil, nil},
    Leaf3 = {1, 10, nil, nil}, 
    % Test merging two leaf nodes
    [?_assertEqual({2, nil, Leaf1, Leaf2}, merge(Leaf1, [Leaf2])),
    % Test merging with an empty list
    ?_assertEqual(Leaf1, [merge(Leaf1, [])]),
    % Test merging with a non-empty list
    ?_assertEqual({2, nil, Leaf1, Leaf3}, merge(Leaf1, [Leaf3]))
    ].

-endif.