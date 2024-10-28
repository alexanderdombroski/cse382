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
add(Trie, [H | T]) ->
    case maps:is_key(H, Trie) of
      true ->
        maps:put(H, add(maps:get(H, Trie), T), Trie);
      false -> 
        maps:put(H, build_branch(#{}, T), Trie) % Sub branch needs to be empty
    end.





-spec build_branch(map(), list()) -> map().
build_branch(Trie, []) ->
    maps:put("End", #{}, Trie); 
build_branch(Trie, [H | T]) -> 
    maps:put(H, build_branch(maps:get(H, Trie, #{}), T), Trie). % Needed to add a default value of #{}




-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").

% Test cases for contains/2
contains_test_() ->
    Trie = maps:from_list([{"a", maps:from_list([{"b", maps:from_list([{"End", #{}}])}])}]),
    [
        ?_assertEqual(true, contains(Trie, ["a", "b"])), % Happy path: key exists.
        ?_assertEqual(false, contains(Trie, ["a", "c"])), % Edge case: partial path exists but fails.
        ?_assertEqual(false, contains(Trie, ["x"])), % Edge case: non-existing root key.
        ?_assertEqual(false, contains(Trie, [])), % Edge case: empty list input for non-empty trie.
        ?_assertEqual(true, contains(maps:from_list([{"End", #{}}]), [])) % Happy path: empty list input with only "End".
    ].

% Test cases for add/2
add_test_() ->
    Trie = #{"a" => #{"b" => #{"End" => #{}}}},
    [
        ?_assertEqual(#{"a" => #{"b" => #{"End" => #{}}}}, add(Trie, ["a", "b"])), % Adding existing path.
        ?_assertEqual(#{"a" => #{"b" => #{"End" => #{}}}, "x" => #{"End" => #{}}}, add(Trie, ["x"])), % Adding new key.
        ?_assertEqual(#{"a" => #{"b" => #{"End" => #{}, "c" => #{"End" => #{}}}}}, add(Trie, ["a", "b", "c"])), % Extending path.
        ?_assertEqual(#{"End" => #{}}, add(#{}, [])), % Edge case: adding empty list to empty trie.
        ?_assertEqual(#{"a" => #{"End" => #{}}}, add(#{}, ["a"])) % Adding a single key to empty trie.
    ].

% Test cases for build_branch/2
build_branch_test_() ->
    Trie = #{},
    [
        ?_assertEqual(#{"a" => #{"b" => #{"End" => #{}}}}, build_branch(Trie, ["a", "b"])), % Happy path: building nested branch.
        ?_assertEqual(#{"End" => #{}}, build_branch(Trie, [])), % Edge case: building branch from empty list.
        ?_assertEqual(#{"a" => #{"End" => #{}}}, build_branch(Trie, ["a"])), % Building branch with one element.
        ?_assertEqual(#{"a" => #{"b" => #{"c" => #{"End" => #{}}}}}, build_branch(Trie, ["a", "b", "c"])) % Deep branch creation.
    ].

-endif.