-module(road_clearance).

-export([generate_vehicle_notifications/3, generate_junction_info/2]).

generate_vehicle_notifications(#{id := RoadId} = RoadSpec, EVs, PreviousNotifications) ->
    [#{
        road_id => RoadId,
        begining_at => 0.0,
        ending_at => 1.0,
        direction => forward,
        notification_body => <<"ev is on your road!">>
    },
    #{
        road_id => RoadId,
        begining_at => 0.0,
        ending_at => 1.0,
        direction => backward,
        notification_body => <<"ev is on your road!">>
    }].

generate_junction_info(RoadSpec, EVs) ->
    [].