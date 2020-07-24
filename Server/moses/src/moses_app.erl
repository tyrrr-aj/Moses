%%%-------------------------------------------------------------------
%% @doc moses public API
%% @end
%%%-------------------------------------------------------------------

-module(moses_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    moses_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
