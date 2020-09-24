-module(dispatcher).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([start_link/1]).


time_res() -> 1000.


%% interface

start_link(EmergencyServiceType) ->
    gen_server:start_link({local, dispatcher_name(EmergencyServiceType)}, ?MODULE, EmergencyServiceType, []).

%% callbacks


init(EmergencyServiceType) ->
    io:format("[Dispatcher] started~n", []),
    DispatchModule = dispatch_module(EmergencyServiceType),
    {ok, Connection} = DispatchModule:establish_connection(),
    {ok, {EmergencyServiceType, DispatchModule, Connection}, time_res()}.

handle_call(_, _, _) ->
    unimplemented.

handle_cast(_, _) ->
    unimplemented.

handle_info(timeout, {EmergencyServiceType, _, _} = State) ->
    % io:format("[Dispatcher] checking for new rides~n"),
    {NewRides, NewState} = check_for_new_rides(State),
    % io:format("[Dispatcher] new rides are: ~p~n", [NewRides]),
    start_trackers_for_new_rides(EmergencyServiceType, NewRides),
    {noreply, NewState, time_res()}.


check_for_new_rides({EmergencyServiceType, DispatchModule, Connection}) ->
    {ok, NewRides, NewConnection} = DispatchModule:get_new_rides(Connection),
    {NewRides, {EmergencyServiceType, DispatchModule, NewConnection}}.

start_trackers_for_new_rides(EmergencyServiceType, NewRides) ->
    lists:foreach(fun(RideData) -> ev_trackers_sup:start_ride(EmergencyServiceType, RideData) end, NewRides).

dispatch_module(EmergencyServiceType) ->
    ServiceTypeAsList = atom_to_list(EmergencyServiceType),
    list_to_atom(ServiceTypeAsList ++ "_dispatch").

dispatcher_name(EmergencyServiceType) ->
    ServiceTypeAsList = atom_to_list(EmergencyServiceType),
    list_to_atom(ServiceTypeAsList ++ "_dispatcher").