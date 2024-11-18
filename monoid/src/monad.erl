-module(monad).

-export([add/2, add/3, contains/2]).




-type bst_node() :: {integer(), bst_node() | nil, bst_node() | nil}.

% Max Heigtht, Node Count
-type bst_meta() :: {integer(), integer()}.





-spec add(integer(), bst_node(), bst_meta()) -> {bst_node(), bst_meta()}.
add(Value, {}) -> 
    {
        {Value, nil, nil},
        {1, 1}
    };
add(Value, {Head, {MaxHeight, NodeCount}}) -> 
    {UpdatedTree, BranchHeight} = r_add(Value, Head, 0),
    {
        UpdatedTree,
        {max(BranchHeight, MaxHeight), NodeCount + 1}
    }.
add(Value, Head, {MaxHeight, NodeCount}) -> 
    {UpdatedTree, BranchHeight} = r_add(Value, Head, 0),
    {
        UpdatedTree,
        {max(BranchHeight, MaxHeight), NodeCount + 1}
    }.


-spec r_add(integer(), bst_node(), integer()) -> bst_node().
r_add(Value, {NodeValue, L, R}, H) when Value < NodeValue ->
    {NL, H2} = r_add(Value, L, H+1),
    {
        {NodeValue, NL, R},
        H2
    };
r_add(Value, {NodeValue, L, R}, H) when Value >= NodeValue ->
    {NR, H2} = r_add(Value, R, H+1),
    {
        {NodeValue, L, NR},
        H2
    };
r_add(Value, nil, H) -> % Add to empty Node
    {
        {Value, nil, nil},
        H+1
    }.






-spec contains(integer(), tuple()) -> boolean().
contains(_, nil) -> false;
contains(Value, {Value, _, _}) -> true;
contains(Value, {NodeValue, L, _}) when Value < NodeValue ->
    contains(Value, L);
contains(Value, {NodeValue, _, R}) when Value > NodeValue ->
    contains(Value, R).









-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").


-endif.