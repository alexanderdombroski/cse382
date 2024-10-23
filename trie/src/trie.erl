-module(trie).

-export([contains/2, add/2, build_branch/2, example/0]).


example() -> #{
        "c" => #{
            "a" => #{
                "t"=> #{
                    "End" => #{}, 
                    "s" => #{"End" => #{}}
                }
            }
        }
    }.


-spec contains(map(), list()) -> boolean(). 
contains(Trie, []) ->
    maps:is_key("End", Trie);
contains(Trie, [H|T]) ->
    case maps:get(H, Trie, undefined) of 
        undefined -> false;
        SubTrie -> contains(SubTrie, T)
    end. 

-spec add(map(), list()) -> map().
add(Trie, []) -> 
    maps:put("End", #{}, Trie);
add(Trie, [H | T]) when maps:is_key(H, Trie) -> 
    add(maps:get(H, Trie), T).





-spec build_branch(map(), list()) -> map().
build_branch(Trie, []) ->
    maps:put("End", #{}, Trie); 
build_branch(Trie, [H | T]) -> 
    maps:put(H, build_branch(maps:get(H, Trie), T), Trie). 




