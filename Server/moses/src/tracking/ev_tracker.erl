-module(ev_tracker).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([start_link/2]).


time_res() -> 1000.


%% API

start_link(EmergencyServiceType, RideData) ->
    gen_server:start_link({local, ev_guardian}, ?MODULE, {EmergencyServiceType, RideData}, []).


%% Callbacks

init({EmergencyServiceType, RideData}) ->
    TrackingModule = tracking_module(EmergencyServiceType),
    TrackingInfo = TrackingModule:start_tracking(RideData),
    add_ev_on_road(maps:get(current_position, TrackingInfo), TrackingInfo),
    {ok, {TrackingModule, TrackingInfo}, time_res()}.

handle_info(timeout, {TrackingModule, TrackingInfo}) ->
    NewTrackingInfo = update_tracking_info(TrackingModule, TrackingInfo),
    update_controllers(NewTrackingInfo),
    {noreply, {TrackingModule, NewTrackingInfo}, time_res()}.

handle_call(_, _, State) ->
    {reply, not_implemented, State}.

handle_cast(_, State) ->
    {noreply, State}.

terminate(normal, TrackingInfo) ->
    cleanup(TrackingInfo);

terminate(shutdown, TrackingInfo) ->
    cleanup(TrackingInfo);

terminate(_Reason, _) ->
    ok.

%% Internal functions

%% TrackingInfo = #{
%%  current_position => {road, RoadId, PartOfRoad} | {junction, JunctionId},
%%  previous_position => {road, RoadId, PartOfRoad} | {junction, JunctionId},
%%  direction => fordard | backward | not_moving | road_changed
%%  ride_id => RideId,
%%  emergency_service_type => EmergencyServiceType 
%% }

tracking_module(EmergencyServiceType) ->
    ServiceType = atom_to_list(EmergencyServiceType),
    list_to_atom(ServiceType ++ "_tracking").

update_tracking_info(TrackingModule, OldTrackingInfo) ->
    OldPosition = maps:get(get_current_position, OldTrackingInfo),
    NewPosition = update_position(TrackingModule, maps:get(ride_identifier, OldTrackingInfo)),
    OldTrackingInfo#{previous_position := OldPosition, current_position := NewPosition, direction := direction(OldPosition, NewPosition)}.

update_position(TrackingModule, RideId) ->
    GPSCoords = TrackingModule:get_current_position(RideId),
    localization:get_position(GPSCoords).

direction({road, Road, OldPartOfRoad}, {road, Road, NewPartOfRoad}) ->
    if
        OldPartOfRoad < NewPartOfRoad -> forward;
        OldPartOfRoad > NewPartOfRoad -> backward;
        true -> not_moving
    end;

direction(_, _) -> road_changed.

update_controllers(#{current_position := {junction, JunctionId}, previous_position := {road, _, _} = OldPosition} = TrackingInfo) ->
    add_ev_on_junction(JunctionId, TrackingInfo),
    remove_ev_from_road(OldPosition, TrackingInfo);

update_controllers(#{current_position := {road, _, _} = NewPosition, previous_position := {junction, JunctionId}} = TrackingInfo) ->
    remove_ev_from_junction(JunctionId, maps:get(ride_id, TrackingInfo)),
    add_ev_on_road(NewPosition, TrackingInfo);

update_controllers(#{current_position := {road, Road, _} = NewPosition, previous_position := {road, Road, _}} = TrackingInfo) ->
    update_ev_position_on_road(NewPosition, TrackingInfo).

add_ev_on_road({road, Road, PartOfRoad}, #{ride_id := RideId, direction := Direction, emergency_service_type := EmergencyServiceType}) ->
    road_controller:add_ev(Road, {RideId, PartOfRoad, Direction, EmergencyServiceType}).

remove_ev_from_road({road, Road, _}, #{ride_id := RideId}) ->
    road_controller:remove_ev(Road, RideId).

update_ev_position_on_road({road, Road, PartOfRoad}, #{ride_id := RideId, direction := Direction}) ->
    if
        Direction == not_moving -> ok;
        true -> road_controller:update_ev_position(Road, {RideId, PartOfRoad, Direction})
    end.

add_ev_on_junction(JunctionId, #{ride_id := RideId}) ->
    junction_controller:add_ev(JunctionId, RideId).

remove_ev_from_junction(JunctionId, RideId) ->
    junction_controller:remove_ev(JunctionId, RideId).

cleanup({TrackingModule, #{current_position := {road, _, _} = CurrentPosition, ride_id := RideId} = TrackingInfo}) ->
    TrackingModule:stop_tracking(TrackingInfo),
    remove_ev_from_road(CurrentPosition, RideId),
    ok;

cleanup({TrackingModule, #{current_position := {junction, _} = CurrentPosition, ride_id := RideId} = TrackingInfo}) ->
    TrackingModule:stop_tracking(TrackingInfo),
    remove_ev_from_junction(CurrentPosition, RideId),
    ok.