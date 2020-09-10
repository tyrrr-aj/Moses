-module(driver_apps_controllern).

-behaviour(gen_server).

-export([handle_call/3, handle_cast/2, init/1]).
-export([start_link/0]).

%% interface

start_link() ->
    gen_server:start_link({local, notification_announcer}, ?MODULE, []).

%% callbacks

init(_) ->
    ok.

handle_call(_, _, _) ->
    ok.

handle_cast(_, _) ->
    ok.
