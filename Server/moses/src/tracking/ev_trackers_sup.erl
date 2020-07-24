-module(ev_trackers_sup).

-behaviour(supervisor).

-export([init/1]).
-export([start_link/0, start_ride/2]).

start_link() ->
    supervisor:start_link({local, ev_trackers_sup}, ?MODULE, []).

start_ride(EmergencyServiceType, RideData) ->
    supervisor:start_child(?MODULE, EmergencyServiceType, RideData).

%% Callbacks

init([]) ->
    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [#{
                        id => ev_tracker,
                        start => {ev_tracker, start_link, []},
                        restart => transient,
                        type => worker
                    }
                ],
    {ok, {SupFlags, ChildSpecs}}. 
