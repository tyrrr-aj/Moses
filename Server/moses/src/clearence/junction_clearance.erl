-module(junction_clearance).

-export([generate_notifications/2, generate_warnings/1, prepare_warnings_to_pass/3]).


%% interface

generate_notifications(JunctionId, EVs) ->
    EvList = maps:to_list(EVs),
    lists:map(fun({RideId, _EmergencyServiceType}) -> notification(JunctionId, RideId) end, EvList).


generate_warnings(#{spec := #{roads := Roads}, evs := EVs}) ->
    EVsWithDistance = maps:map(fun(_, EmergencyServiceType) -> #{
            emergency_service_type => EmergencyServiceType,
            remaining_distance => constants:notifying_range()
        } end, EVs),
    [{Road, EVsWithDistance} || Road <- Roads].


prepare_warnings_to_pass(#{roads := Roads}, RoadId, EVs) ->
    TargetRoads = lists:filter(fun(Road) -> Road /= RoadId end, Roads),
    [{Road, EVs} || Road <- TargetRoads].


%% internal functions

notification(JunctionId, RideId) ->
    #{
        ride_id => RideId,
        road_network_element_type => junction,
        road_network_element_id => JunctionId,
        notification_code => notification_codes:make_way_on_junction_code()
    }.
