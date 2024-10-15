-module(paren_checker).
-export([start/1]).

-spec start(String::string()) -> ok.
start(String) -> 
    Tuples = tear_apart_string(String),
    find_paren(Tuples),
    ok.
    

% Hello -> {H, {e, {l, {l, {o, nil}}}}}
tear_apart_string([]) -> nil;
tear_apart_string([H | T]) ->
    {H, tear_apart_string(T)};
tear_apart_string(_) ->
    unknown_args.

find_paren(nil) -> ok;
find_paren({C, Tuples}) -> find_paren({C, Tuples}, {}).

find_paren(nil, {}) ->
    io:format("Checks Finished~n");
find_paren(nil, {C, Stack}) ->
    io:format("Unclosed: ~c~n", [C]),
    find_paren(nil, Stack);
find_paren({C, Tuples}, Stack) when C == 123; C == 91; C == 40 ->    
    find_paren(Tuples, {C, Stack});
find_paren({93, Tuples}, {91, Stack}) -> % []
    find_paren(Tuples, Stack);
find_paren({41, Tuples}, {40, Stack}) -> % ()
    find_paren(Tuples, Stack);
find_paren({125, Tuples}, {123, Stack}) -> % {}
    find_paren(Tuples, Stack);
find_paren({C, Tuples}, Stack) when C == 93; C == 41; C == 125 ->
    io:format("error: ~c~n", [C]), % The reason for brackets is it's a list of args.
    find_paren(Tuples, Stack);
find_paren({_, Tuples}, Stack) -> find_paren(Tuples, Stack).

