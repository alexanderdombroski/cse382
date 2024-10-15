%%%-------------------------------------------------------------------
%% @doc mylib public API
%% 
%% This is my cool program my rebar wrote me :)
%% Thanks Rebar. You're the best!
%% 
%% @end
%%%-------------------------------------------------------------------

-module(mylib_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    mylib_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
