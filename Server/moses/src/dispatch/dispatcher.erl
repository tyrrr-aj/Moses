-module(dispatcher).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([start_link/1]).


time_res() -> 1000.


%% interface

start_link(EmergencyServiceType) ->
    gen_server:start_link({local, dispatcher_name(EmergencyServiceType)}, ?MODULE, [EmergencyServiceType], []).

%% callbacks


init(EmergencyServiceType) ->
    DispatchModule = dispatch_module(EmergencyServiceType),
    Connection = DispatchModule:establish_connection(),
    {ok, {EmergencyServiceType, DispatchModule, Connection}, time_res()}.

handle_call(_, _, _) ->
    unimplemented.

handle_cast(_, _) ->
    unimplemented.

handle_info(timeout, State) ->
    check_for_new_rides(State),
    {noreply, State}.


check_for_new_rides({EmergencyServiceType, DispatchModule, Connection}) ->
    NewRides = DispatchModule:get_new_rides(Connection),
    lists:foreach(fun(RideData) -> ev_trackers_supervisor:start_ride(EmergencyServiceType, RideData) end, NewRides),
    ok.

dispatch_module(EmergencyServiceType) ->
    ServiceTypeAsList = atom_to_list(EmergencyServiceType),
    list_to_atom(ServiceTypeAsList ++ "_dispatch").

dispatcher_name(EmergencyServiceType) ->
    ServiceTypeAsList = atom_to_list(EmergencyServiceType),
    list_to_atom(ServiceTypeAsList ++ "_dispatcher").