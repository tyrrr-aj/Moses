-module(road_clearance).

-export([generate_vehicle_notifications/2, generate_info_for_junctions/2, prepare_info_from_junction_to_pass/3, pass_info_for_junctions/2, 
    generate_notifications_for_vehicles_coming_to_junction/3, retrieve_passed_evs/3]).


%% interface

%%     generating notifications

generate_vehicle_notifications(RoadSpec, EVs) ->
    EVsWithKnownDirection = maps:filter(fun(_, #{direction := Direction}) -> (Direction == forward) or (Direction == backward) end, EVs),
    EVsWithRemainingDistance = set_initial_distances(EVsWithKnownDirection),
    [make_way_on_road_notification(RoadSpec, Entry) || Entry <- maps:to_list(EVsWithRemainingDistance)] ++
    [action_unnecessary_notification(RoadSpec, Entry) || Entry <- maps:to_list(EVsWithRemainingDistance)].


generate_notifications_for_vehicles_coming_to_junction(RoadSpec, JunctionId, EVs) ->
    Direction = direction_to_junction(RoadSpec, JunctionId),
    CorrectedEVs = set_reversed_direction_and_position(EVs, Direction),
    [make_way_on_junction_notification(RoadSpec, Ev) || Ev <- maps:to_list(CorrectedEVs)].


%%     passing info for junctions

generate_info_for_junctions(RoadSpec, EVs) ->
    EVsWithRemainingDistance = set_initial_distances(EVs),
    pass_info_for_junctions(RoadSpec, EVsWithRemainingDistance).


prepare_info_from_junction_to_pass(RoadSpec, JunctionId, EVs) ->
    Direction = direction_to_junction(RoadSpec, JunctionId),
    CorrectedEVs = set_reversed_direction_and_position(EVs, Direction),
    junction_info(RoadSpec, CorrectedEVs, opposite_direction(Direction)).
    


pass_info_for_junctions(RoadSpec, EVs) ->
    junction_info(RoadSpec, EVs, forward) ++ junction_info(RoadSpec, EVs, backward).


%%    receiving info from junctions
retrieve_passed_evs(RoadSpec, JunctionId, EVs) ->
    Direction = opposite_direction(direction_to_junction(RoadSpec, JunctionId)),
    set_direction_and_position(EVs, Direction).


%% private functions

make_way_on_road_notification(#{id := RoadId} = RoadSpec, {RideId,
    #{
        direction := Direction,
        emergency_service_type := EmergencyServiceType} = Ev
    }) ->
        #{
        ride_id => RideId,
        road_network_element_type => road,
        road_network_element_id => RoadId,
        beginning_at => begin_at(Ev, RoadSpec),
        ending_at => end_at(Ev, RoadSpec),
        direction => Direction,
        notification_code => notification_codes:make_way_on_road_code()
        }.


action_unnecessary_notification(#{id := RoadId} = RoadSpec, {RideId,
    #{
        direction := Direction,
        emergency_service_type := EmergencyServiceType} = Ev
    }) ->
        #{
        ride_id => RideId,
        road_network_element_type => road,
        road_network_element_id => RoadId,
        beginning_at => begin_at(Ev, RoadSpec),
        ending_at => end_at(Ev, RoadSpec),
        direction => opposite_direction(Direction),
        notification_code => notification_codes:action_unnecessary_code()
        }.

make_way_on_junction_notification(#{id := RoadId} = RoadSpec, {RideId, #{direction := Direction} = Ev}) ->
    #{
        ride_id => RideId,
        road_network_element_type => road,
        road_network_element_id => RoadId,
        beginning_at => begin_at(Ev, RoadSpec),
        ending_at => end_at(Ev, RoadSpec),
        direction => opposite_direction(Direction),
        notification_code => notification_codes:make_way_on_junction_code()
    }.


junction_info(#{id := RoadId} = RoadSpec, EVs, Direction) ->
    case junction(RoadSpec, Direction) of
        dead_end -> [];
        JunctionId ->
            IncomingEVs = evs_in_direction(EVs, Direction),
            [#{
                junction_id => JunctionId,
                road_id => RoadId,
                evs => filter_out_finished(maps:map(fun(_, Ev) -> ev_info(Ev, RoadSpec, Direction) end, IncomingEVs))
            }]
    end.


ev_info(#{part_of_road := PartOfRoad, 
          remaining_distance := PreviousRemainingDistance,
          emergency_service_type := EmergencyServiceType}, 
        RoadSpec, Direction) ->
    NewRemainingDistance = remaining_distance(PreviousRemainingDistance, RoadSpec, PartOfRoad, Direction),
    case NewRemainingDistance > 0.0 of
        true -> #{
                    remaining_distance => NewRemainingDistance,
                    emergency_service_type => EmergencyServiceType
                };
        false -> finished
    end.


set_initial_distances(EVs) ->
    maps:map(fun(_, Ev) -> Ev#{remaining_distance => constants:notifying_range()} end, EVs).


set_direction_and_position(EVs, Direction) ->
    PartOfRoad = case Direction of
        forward -> 0.0;
        backward -> 1.0
    end,
    maps:map(fun(_, Ev) -> Ev#{direction => Direction, part_of_road => PartOfRoad} end, EVs).


set_reversed_direction_and_position(EVs, Direction) ->
    PartOfRoad = case Direction of
        forward -> 1.0;
        backward -> 0.0
    end,
    maps:map(fun(_, Ev) -> Ev#{direction => opposite_direction(Direction), part_of_road => PartOfRoad} end, EVs).


evs_in_direction(EVs, DesiredDirection) ->
    maps:filter(fun(_, #{direction := EvDirection}) -> (EvDirection == DesiredDirection) end, EVs).


filter_out_finished(EvInfos) ->
    maps:filter(fun(_, EvInfo) -> EvInfo /= finished end, EvInfos).


junction(#{ending_at := JunctionId}, forward) -> JunctionId;

junction(#{beginning_at := JunctionId}, backward) -> JunctionId.


begin_at(#{direction := forward, part_of_road := PartOfRoad}, _) -> PartOfRoad;

begin_at(#{direction := backward, remaining_distance := RemainingDistance, part_of_road := PartOfRoad}, #{length := RoadLength}) -> 
    case RemainingDistance > RoadLength * PartOfRoad of 
        true -> 0.0;
        false -> PartOfRoad - RemainingDistance / RoadLength
    end.


end_at(#{direction := forward, remaining_distance := RemainingDistance, part_of_road := PartOfRoad}, #{length := RoadLength}) -> 
    case RemainingDistance > RoadLength * (1.0 - PartOfRoad) of 
        true -> 1.0;
        false -> PartOfRoad + RemainingDistance / RoadLength
    end;

end_at(#{direction := backward, part_of_road := PartOfRoad}, _) -> PartOfRoad.


opposite_direction(forward) -> backward;

opposite_direction(backward) -> forward.


remaining_distance(PreviousRemainingDistance, #{length := Length}, PartOfRoad, forward) ->
    io:format("[remaining_distance] PreviousDistance = ~p, Length = ~p, PartOfRoad = ~p, forward, ", [PreviousRemainingDistance, Length, PartOfRoad]),
    Result = PreviousRemainingDistance - Length * (1.0 - PartOfRoad),
    io:format(" result = ~p~n", [Result]),
    Result;

remaining_distance(PreviousRemainingDistance, #{length := Length}, PartOfRoad, backward) ->
    io:format("[remaining_distance] PreviousDistance = ~p, Length = ~p, PartOfRoad = ~p, backward, ", [PreviousRemainingDistance, Length, PartOfRoad]),
    Result = PreviousRemainingDistance - Length * PartOfRoad,
    io:format(" result = ~p~n", [Result]),
    Result.


direction_to_junction(#{beginning_at := JunctionId}, JunctionId) ->
    backward;

direction_to_junction(#{ending_at := JunctionId}, JunctionId) ->
    forward.
