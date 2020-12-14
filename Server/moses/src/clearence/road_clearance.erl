-module(road_clearance).

-export([generate_vehicle_notifications/3, generate_junction_info/2]).

generate_vehicle_notifications(#{id := RoadId} = RoadSpec, EVs, PreviousNotifications) ->
    EVsWithKnownDirection = maps:filter(fun(_, #{direction := Direction}) -> (Direction == forward) or (Direction == backward) end, EVs),
    [action_required_notification(RoadId, Entry) || Entry <- maps:to_list(EVsWithKnownDirection)].
    % ++ [action_unnecessary_notification(RoadId, Entry) || Entry <- maps:to_list(EVsWithKnownDirection)].


generate_junction_info(RoadSpec, EVs) ->
    [].


%% private functions

action_required_notification(RoadId, {RideId,
    #{
        direction := Direction,
        emergency_service_type := EmergencyServiceType} = Ev
    }) ->
        #{
        ride_id => RideId,
        road_network_element_type => road,
        road_network_element_id => RoadId,
        begining_at => begin_at(Ev),
        ending_at => end_at(Ev),
        direction => Direction,
        notification_body => action_required_message()
        }.


action_unnecessary_notification(RoadId, {RideId,
    #{
        direction := Direction,
        emergency_service_type := EmergencyServiceType} = Ev
    }) ->
        #{
        ride_id => RideId,
        road_network_element_type => road,
        road_network_element_id => RoadId,
        begining_at => begin_at(Ev),
        ending_at => end_at(Ev),
        direction => opposite_direction(Direction),
        notification_body => action_unnecessary_message()
        }.


begin_at(#{direction := forward, part_of_road := PartOfRoad}) -> PartOfRoad;

begin_at(#{direction := backward}) -> 0.0.


end_at(#{direction := forward}) -> 1.0;

end_at(#{direction := backward, part_of_road := PartOfRoad}) -> PartOfRoad.


opposite_direction(forward) -> backward;

opposite_direction(backward) -> forward.


%% private constants

action_required_message() -> <<"Ev is coming - make way!">>.

action_unnecessary_message() -> <<"Ev is near, but you don't have to do anything">>.
