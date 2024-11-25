% GANG OF FOUR STUDY

-module(gang).

-export([start_event_emitter/0, start_listener/0]).



% Event Emitter Process
start_event_emitter() ->
    spawn(fun event_emitter/0).

event_emitter() ->
    receive
        {register_listener, ListenerPid} ->
            listen([ListenerPid]);
        {event, EventData} ->
            io:format("Unhandled event: ~p~n", [EventData]),
            event_emitter()
    end.

listen(Listeners) ->
    receive
        {register_listener, ListenerPid} ->
            listen([ListenerPid | Listeners]);
        {event, EventData} ->
            lists:foreach(fun(ListenerPid) ->
                ListenerPid ! {event, EventData}
            end, Listeners),
            listen(Listeners);
        stop ->
            ok
    end.

% Listener Example
start_listener() ->
    spawn(fun listener/0).

listener() ->
    receive
        {event, Data} ->
            io:format("Listener received event: ~p~n", [Data]),
            listener()
    end.





-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").



-endif.