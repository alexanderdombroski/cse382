-module(trie).

-export([contains/2, add/2, build_branch/2]).


-spec contains(map(), list()) -> boolean(). 
contains(Trie, []) ->
    maps:is_key("End", Trie);
contains(Trie, [H|T]) ->
    case maps:get(H, Trie, undefined) of 
        undefined -> false;
        subTrie -> contains(subTrie, T)
    end. 

-spec add(map(), list()) -> map().
add(Trie, Sequence) -> ok.

-spec build_branch(map(), list()) -> map().
build_branch(Trie, Sequence) -> ok.




