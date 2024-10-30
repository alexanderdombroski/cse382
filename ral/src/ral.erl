-module(ral).

-export([get/1, find_tree/1, build_traversal_list/1, find_using_traversal_list/1, update/1, replace_using_traversal_list/1, cons/2, merge/2]).


-type leaf_node() :: {1, integer(), nil, nil}.

% { number of leaf nodes, node, node } 
-type parent_node() :: {integer(), parent_node() | leaf_node(), parent_node() | leaf_node()}.

-spec get(integer()) -> integer().
get([]) -> nil;
get(_) -> ok.

-spec find_tree(integer()) -> tuple().
find_tree(_) -> ok.


-spec build_traversal_list(integer()) -> tuple().
build_traversal_list(_) -> ok.



-spec find_using_traversal_list(list()) -> integer().
find_using_traversal_list(_) -> ok.


% Recursive??
-spec update(tuple()) -> list().
update(_) -> ok.

-spec replace_using_traversal_list(list()) -> integer().
replace_using_traversal_list([]) -> ok.


% Adds item to the beginning
-spec cons(integer(), list()) -> ok.
cons(V, nil) -> 
    cons(V, []);
cons(V, []) -> 
    [{1, V, nil, nil}];
cons(V, [{1, NV, nil, nil} | T]) -> 
    [merge({1, V, nil, nil}, {1, NV, nil, nil}), T]; % Need to better handle mergeing for all even trees
cons(V, Ral) ->
    {1, V, nil, nil} ++ Ral.


-spec merge(list(), list()) -> list().
merge({Count, LV, LL, LR}, {Count, RV, RL, RR}) -> 
    {Count + Count, {Count, LV, LL, LR}, {Count, RV, RL, RR}};
merge({Count, LL, LR}, {Count, RL, RR}) -> 
    {Count + Count, {Count, LL, LR}, {Count, RL, RR}}.





-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").







-endif.