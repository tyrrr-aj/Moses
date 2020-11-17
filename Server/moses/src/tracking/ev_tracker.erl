-module(ev_tracker).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([start_link/2]).


%% TrackingInfo = #{
%%  current_position => {road, RoadId, PartOfRoad} | {junction, JunctionId} | unknown,
%%  previous_position => {road, RoadId, PartOfRoad} | {junction, JunctionId} | unknown,
%%  direction => forward | backward | not_moving | road_changed
%%  ride_id => RideId,
%%  emergency_service_type => EmergencyServiceType
%%  connection => Connection
%% }

time_res() -> 1000.


%% API

start_link(EmergencyServiceType, #{ride_id := RideId}) ->
    gen_server:start_link(?MODULE, {EmergencyServiceType, RideId}, []).


%% Callbacks

init({EmergencyServiceType, RideId}=TrackingSpec) ->
    io:format("Ev_tracker started~n", []),
    TrackingModule = tracking_module(TrackingSpec),
    {ok, InitialCoords} = TrackingModule:start_tracking(RideId),
    % add_ev_on_road(maps:get(current_position, TrackingInfo), TrackingInfo),
    InitialPosition = get_position(InitialCoords),
    TrackingInfo = init_tracking_info(RideId, EmergencyServiceType, InitialPosition, TrackingModule),
    {ok, TrackingInfo, time_res()}.


handle_info(timeout, TrackingInfo) ->
    % io:format("[Ev_tracker] Updating position...~n", []),
    NewTrackingInfo = update_tracking_info(TrackingInfo),
    % io:format(" current position: ~p, direction: ~p~n", [maps:get(current_position, NewTrackingInfo), maps:get(direction, NewTrackingInfo)]),
    % io:format("[update_controllers] will be called with ~p~n", [NewTrackingInfo]),
    update_controllers(NewTrackingInfo),
    {noreply, NewTrackingInfo, time_res()}.


handle_call(_, _, _) ->
    should_not_be_used.


handle_cast(_, _) ->
    should_not_be_used.


terminate(normal, TrackingInfo) ->
    cleanup(TrackingInfo);

terminate(shutdown, TrackingInfo) ->
    cleanup(TrackingInfo);

terminate(_Reason, _) ->
    ok.

%% Internal functions


tracking_module(EmergencyServiceType) ->
    ServiceType = atom_to_list(EmergencyServiceType),
    list_to_atom(ServiceType ++ "_tracking").


init_tracking_info(RideId, EmergencyServiceType, Position, TrackingModule) ->
    #{
        ride_id => RideId,
        current_position => Position,
        previous_position => unknown,
        direction => not_moving,
        emergency_service_type => EmergencyServiceType,
        connection => TrackingModule
    }.


update_tracking_info(#{current_position := OldPosition}=OldTrackingInfo) ->
    case update_position(OldTrackingInfo) of
        unknown ->
            OldTrackingInfo;
        NewPosition -> 
            OldTrackingInfo#{
                previous_position := OldPosition,
                current_position := NewPosition,
                direction := direction(OldPosition, NewPosition)
            }
    end.


update_position(#{ride_id := RideId, connection := TrackingModule}) ->
    {ok, GPSCoords} = TrackingModule:get_current_position(RideId),
    % io:format("Coordinates for EV: ~p~n", [GPSCoords]),
    get_position(GPSCoords).


get_position(unknown) ->
    unknown;

get_position(GPSCoords) ->
    map_controller:get_position(GPSCoords).


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
    % io:format("[update_controllers] updateing ev's position on road~n", []),
    update_ev_position_on_road(NewPosition, TrackingInfo);

update_controllers(#{current_position := {road, _, _} = NewPosition, previous_position := {road, _, _} = OldPosition} = TrackingInfo) ->
    % io:format("[update_controllers] changing ev's road~n", []),
    remove_ev_from_road(OldPosition, TrackingInfo),
    add_ev_on_road(NewPosition, TrackingInfo);

update_controllers(#{current_position := {road, _, _} = NewPosition, previous_position := unknown} = TrackingInfo) ->
    % io:format("[update_controllers] adding ev on road~n", []),
    add_ev_on_road(NewPosition, TrackingInfo);

update_controllers(#{current_position := unknown}) ->
    % io:format("[update_controllers] ev position unknown~n", []),
    ok.


add_ev_on_road({road, Road, PartOfRoad}, #{ride_id := RideId, direction := Direction, emergency_service_type := EmergencyServiceType}) ->
    road_controller:add_ev(Road, {RideId, PartOfRoad, Direction, EmergencyServiceType}).


remove_ev_from_road({road, Road, _}, #{ride_id := RideId}) ->
    road_controller:remove_ev(Road, RideId).


update_ev_position_on_road({road, Road, PartOfRoad}, #{ride_id := RideId, direction := Direction}) ->
    road_controller:update_ev_position(Road, {RideId, PartOfRoad, Direction}).


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