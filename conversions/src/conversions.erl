-module(conversions).

-export([structure_map/2]).

-type callback() :: fun((term()) -> term()).

-type ral() :: list(node()).
-type ral_node() :: {integer(), integer() | nil, ral_node() | nil, ral_node() | nil}.

-type bst_node() :: {integer(), bst_node() | nil, bst_node() | nil}.

-type heap_node() :: {integer(), integer(), heap_node() | nil, heap_node() | nil}.

-type rbt_node() :: {black | red, integer(), rbt_node() | nil, rbt_node() | nil}.

-type trie_node() :: map().

%% Specification

% structure_map/2 applies a function to a data structure and returns a transformed structure.
%
% @spec structure_map(Function, Data) -> Result
%    where
%      Function :: (term() -> term()), 
%      Data :: list() | ral() | bst_node() | 
%              heap_node() | rbt_node() | ral_node() | map()
%      Result :: list() | ral() | bst_node() | 
%                heap_node() | rbt_node() | ral_node() | map().
%
% @param Function The function to apply to each element in the data structure.
% @param Data The data structure to be transformed, which can be various types.
% @return A new data structure of the same type, with the function applied to its elements.

-spec structure_map(
    Function :: callback(), 
    Data :: list() | ral() | bst_node() | 
        heap_node() | rbt_node() | trie_node() | ral_node()
    ) -> list() | ral() | bst_node() | heap_node() | 
        rbt_node()  | trie_node() | ral_node().

structure_map(_, nil) -> % List
    nil;
structure_map(_, []) -> % List
    [];
structure_map(Fun, Trie) when is_map(Trie) -> % Trie
    maps:fold(
        fun(K, V, Acc) ->  
        maps:put(Fun(K), structure_map(Fun, V), Acc)
    end, #{}, Trie
    ); 
structure_map(Fun, [{C, V, L, R} | T]) -> % Ral
    [{C, Fun(V), structure_map(Fun, L), structure_map(Fun, R)} | structure_map(Fun, T)];
structure_map(Fun, [H | T]) -> % List
    [Fun(H) | structure_map(Fun, T)];
structure_map(Fun, {V, L, R}) -> % bst
    {Fun(V), structure_map(Fun, L), structure_map(Fun, R)};
structure_map(Fun, {C, V, L, R}) -> % rbt, heap, or ral_node
    {C, Fun(V), structure_map(Fun, L), structure_map(Fun, R)}.





-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").

% Sample callback functions
increment(X) -> X + 1.
double(X) -> X * 2.

capitalize(Str) when is_list(Str), Str =/= [] ->
    [First | Rest] = Str,
    string:to_upper([First]) ++ string:to_lower(Rest);
capitalize([]) ->
    [].

% Test for list structure
list_test_() ->
    [
        ?_assertEqual([], structure_map(fun increment/1, [])),
        ?_assertEqual([2, 3, 4], structure_map(fun increment/1, [1, 2, 3]))
    ].

% Test for random access list (RAL)
ral_test_() ->
    RAL = [{1, 2, [], []}, {2, 4, [{1, 2, [], []}], [{1, 3, [], []}]}],
    ExpectedRAL = [{1, 3, [], []}, {2, 5, [{1, 3, [], []}], [{1, 4, [], []}]}],
    [
        ?_assertEqual(ExpectedRAL, structure_map(fun(X) -> X + 1 end, RAL))
    ].
% Test for BST
bst_test_() ->
    BST = {4, {2, nil, nil}, {6, nil, nil}},
    ExpectedBST = {5, {3, nil, nil}, {7, nil, nil}},
    [
        ?_assertEqual(ExpectedBST, structure_map(fun(X) -> X + 1 end, BST))
    ].
% Test for heap
heap_test_() ->
    Heap = {1, 10, nil, nil},
    ExpectedHeap = {1, 20, nil, nil},
    [
        ?_assertEqual(ExpectedHeap, structure_map(fun double/1, Heap)),
        ?_assertEqual({2, 3, {1, 8, nil, nil}, {1, 22, nil, nil}}, structure_map(fun increment/1, {2, 2, {1, 7, nil, nil}, {1, 21, nil, nil}}))
    ].

% Test for red-black tree
rbt_test_() ->
    RBT = {black, 3, {red, 2, nil, nil}, {red, 5, nil, nil}},
    ExpectedRBT = {black, 4, {red, 3, nil, nil}, {red, 6, nil, nil}},
    [
        ?_assertEqual(ExpectedRBT, structure_map(fun increment/1, RBT))
    ].
% Test for trie
trie_test_() ->
    Trie = #{
        "c" => #{
            "a" => #{
                "t"=> #{
                    "End" => #{}, 
                    "s" => #{"End" => #{}}
                }
            }
        }
    },
    ExpectedTrie = #{
        "C" => #{
            "A" => #{
                "T"=> #{
                    "End" => #{}, 
                    "S" => #{"End" => #{}}
                }
            }
        }
    },
    [
        ?_assertEqual(ExpectedTrie, structure_map(fun capitalize/1, Trie))
    ].

-endif.