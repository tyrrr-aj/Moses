-module(dispatcher).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([start_link/1]).


time_res() -> 1000.


%% interface

start_link(EmergencyServiceType) ->
    gen_server:start_link({local, names:dispatcher_name(EmergencyServiceType)}, ?MODULE, EmergencyServiceType, []).

%% callbacks


init(EmergencyServiceType) ->
    io:format("[Dispatcher] started~n", []),
    DispatchModule = names:dispatch_module(EmergencyServiceType),
    {ok, Connection} = DispatchModule:establish_connection(),
    {ok, {EmergencyServiceType, DispatchModule, Connection}, time_res()}.

handle_call(_, _, _) ->
    unimplemented.

handle_cast(_, _) ->
    unimplemented.

handle_info(timeout, {EmergencyServiceType, DispatchModule, Connection} = State) ->
    % io:format("[Dispatcher] checking for new rides~n"),
    {NewRides, EndedRides} = check_for_new_rides(DispatchModule, Connection),
    % io:format("[Dispatcher] new rides are: ~p~n", [NewRides]),
    start_trackers_for_new_rides(EmergencyServiceType, NewRides),
    stop_trackers_for_ended_rides(EndedRides),
    {noreply, State, time_res()}.


check_for_new_rides(DispatchModule, Connection) ->
    {ok, NewRides} = DispatchModule:get_new_rides(Connection),
    {ok, EndedRides} = DispatchModule:get_ended_rides(Connection),
    {NewRides, EndedRides}.

start_trackers_for_new_rides(EmergencyServiceType, NewRides) ->
    lists:foreach(fun(RideData) -> ev_trackers_sup:start_ride(EmergencyServiceType, RideData) end, NewRides).


stop_trackers_for_ended_rides(EndedRides) ->
    lists:foreach(fun(RideId) -> ev_tracker:stop_tracking(RideId) end, EndedRides).
