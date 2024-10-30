-module(ral).

-export([get/2, find_tree/2, build_traversal_list/1, find_using_traversal_list/2, update/1, replace_using_traversal_list/1, cons/2]).




-type random_access_list() :: list(node()).

-type ral_node() :: {integer(), integer() | nil, ral_node() | nil, ral_node() | nil}.


% Takes an index and a random access list and returns the value at that index
-spec get(integer(), random_access_list()) -> integer().
get(_, []) -> nil;
get(Index, [{Count, _, _, _} | T]) when Index > Count -> 
    get(Index, T);
get(Index, [{Count, V, L, R} | _]) ->
    IntPath = int_to_bitarray(Count - Index).



% -spec find_tree(integer(), random_access_list()) -> ral_node().
% find_tree(_, []) -> nil;
% find_tree(Index, [{Count, _, _, _} | T]) when Index > Count -> 
%     find_tree(Index - Count, T);
% find_tree(_, [H | _]) ->
%     H.

int_to_bitarray(Int) -> 
    BinaryString = integer_to_list(Int, 2),
    lists:map(fun ($1) -> 1; ($0) -> 0 end, BinaryString).


-spec build_traversal_list(ral_node()) -> list(integer()).
build_traversal_list(_) -> ok.



-spec find_using_traversal_list(list(), list(integer())) -> integer().
find_using_traversal_list({_, Value, L, R}, [Instruction, T]) -> 
    ok.


% Recursive??
-spec update(tuple()) -> list().
update(_) -> ok.

-spec replace_using_traversal_list(list()) -> integer().
replace_using_traversal_list([]) -> ok.


% Adds item to the beginning
-spec cons(integer(), random_access_list()) -> ok.
cons(V, nil) -> 
    [{1, V, nil, nil}];
cons(V, []) -> 
    [{1, V, nil, nil}];
cons(V, [{Count, NV, nil, nil} | T]) -> 
    [merge({1, V, nil, nil}, {Count, NV, nil, nil}), T]; 
cons(V, Ral) ->
    {1, V, nil, nil} ++ Ral.



-spec merge(ral_node(), ral_node()) -> ral_node().
merge(Node, []) -> Node;
merge({Count, LV, LL, LR}, [{Count, RV, RL, RR} | T]) -> 
    merge({Count + Count, nil, {Count, LV, LL, LR}, {Count, RV, RL, RR}}, T);
merge(Node, Ral) ->
    Node ++ Ral.


-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").







-endif.