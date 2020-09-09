-module(ev_trackers_sup).

-behaviour(supervisor).

-export([init/1]).
-export([start_link/0, start_ride/2]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_ride(EmergencyServiceType, RideData) ->
    io:format("[Ev-trackers_SUP] Starting new ride for {~p, ~p}~n", [EmergencyServiceType, RideData]),
    {ok, _} = supervisor:start_child(?MODULE, [EmergencyServiceType, RideData]).

%% Callbacks

init([]) ->
    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [
                    #{
                        id => ev_tracker,
                        start => {ev_tracker, start_link, []},
                        restart => transient,
                        type => worker,
                        modules => [ev_tracker]
                    }
                ],
    {ok, {SupFlags, ChildSpecs}}. 
