-module(junction_controller).

-behaviour(gen_server).

-export([handle_call/3, handle_cast/2, init/1]).
-export([start_link/1, warn_of_incoming_evs/1, add_ev/2, remove_ev/2]).

%% Passed State components:
%%
%% spec => #{
%%  id => JunctionId,
%%  roads => [RoadId],
%%  type => plain | traffic_lights | circle,
%%  controller => TrafficLightsController
%% }
%%
%% evs => [RideId]
%%
%% ev_warnings => #{
%%  RoadId => Distance
%% }

%% EVsInfo = {RoadId, #{RideId => Distance}}

%% interface

start_link(#{id := JunctionId} = JunctionSpec) ->
    gen_server:start_link({local, JunctionId}, ?MODULE, [JunctionSpec], []).

add_ev(JunctionId, RideId) ->
    gen_server:cast(JunctionId, {add_ev, RideId}).

remove_ev(JunctionId, RideId) ->
    gen_server:cast(JunctionId, {remove_ev, RideId}).

warn_of_incoming_evs(#{junction_id := JunctionId} = EvsInfo) ->
    gen_server:cast(JunctionId, {update_ev_warnings, EvsInfo}).

%% callbacks

init(JunctionSpec) ->
    case maps:get(type, JunctionSpec) of
        traffic_lights -> 
            Controller = maps:get(controller, JunctionSpec),
            Connected = Controller:setup(maps:get(id, JunctionSpec)),
            {ok, {JunctionSpec#{controller := Connected}, #{}, #{}}};
        _ -> {ok, {JunctionSpec, #{}, #{}}}
    end.

handle_call(_, _, _) ->
    unimplemented.

handle_cast({add_ev, RideId}, #{evs := OldEVs} = State) ->
    NewState = State#{evs := OldEVs ++ RideId},
    FinalState = reevaluate_junction_setting(NewState),
    {ok, FinalState};

handle_cast({remove_ev, RideId}, #{evs := OldEVs} = State) ->
    NewState = State#{evs := lists:delete(RideId, OldEVs)},
    FinalState = reevaluate_junction_setting(NewState),
    {ok, FinalState};

handle_cast({update_ev_warnings, EVsInfo}, #{ev_warnings := EVWarnings} = State) ->
    NewState = State#{ev_warnings := update_ev_warnings(EVsInfo, EVWarnings)},
    FinalState = reevaluate_junction_setting(NewState),
    {ok, FinalState}.


%% internal functions

update_ev_warnings({RoadId, Distances}, EVWarnings) ->
    unimplemented.

reevaluate_junction_setting(State) ->
    unimplemented.