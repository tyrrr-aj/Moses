-module(road_clearance).

-export([generate_vehicle_notifications/3, generate_junction_info/2]).

generate_vehicle_notifications(#{id := RoadId} = RoadSpec, EVs, PreviousNotifications) ->
    [#{
        ride_id => RideId,
        road_network_element_type => road,
        road_network_element_id => RoadId,
        begining_at => 0.0,
        ending_at => 1.0,
        direction => forward,
        notification_body => <<"ev is on your road!">>
    } || RideId <- maps:keys(EVs)] ++
    [#{
        ride_id => RideId,
        road_network_element_type => road,
        road_network_element_id => RoadId,
        begining_at => 0.0,
        ending_at => 1.0,
        direction => backward,
        notification_body => <<"ev is on your road!">>
    } || RideId <- maps:keys(EVs)].

generate_junction_info(RoadSpec, EVs) ->
    [].