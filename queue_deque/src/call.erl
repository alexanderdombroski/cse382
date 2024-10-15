
-module(call).
-export([start/0, assign_call/1, add_call/2, add_priority_call/2]).



%% 
%% 
%% Call Center 
%% 
%% 

start() -> 
    Calls = {[],[]},
    A = deque:assign_call(Calls),
    io:format("~n"),
    
    B = deque:add_call(A, "208-345-6789"),
    io:format("~n"),

    C = deque:add_call(B, "208-492-9999"),

    D = deque:add_priority_call(C, "208-111-2222"),

    E = deque:add_call(D, "208-101-FOOD"),
    io:format("~n"),

    F = deque:assign_call(E),
    io:format("~n"),
    deque:assign_call(F),
    io:format("~n").
    

    
assign_call(Queue) -> 
    io:format(queue_deque:head(Queue) ++ "got assigned~n"),
    queue_deque:dequeue(Queue).

add_call(Queue, Number) -> 
    queue_deque:enqueue(Queue, Number).
    
add_priority_call(Queue, Number) -> 
    queue_deque:enqueue_front(Queue, Number).

make_fun_of_person_that_has_to_wait_forever(Queue) ->
    io:format(queue_deque:tail(Queue) ++ "Has to wait forever~n").