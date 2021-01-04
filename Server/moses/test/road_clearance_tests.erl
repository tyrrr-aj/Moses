-module(road_clearance_tests).

-include_lib("eunit/include/eunit.hrl").


basic_notifications_test() ->
    Notifications = road_clearance:generate_vehicle_notifications(mock_road_spec(), mock_evs()),
    SortedNotifications = lists:sort(fun(N1, N2) -> compare_notifications(N1, N2) end, Notifications),
    SortedExpected = lists:sort(fun(N1, N2) -> compare_notifications(N1, N2) end, basic_notifications_list()),
    Pairs = lists:zip(SortedExpected, SortedNotifications),
    lists:foreach(fun({Got, Expected}) -> ?assertEqual(Expected, Got) end, Pairs).
    

notifications_for_vehicles_coming_to_junction_test() ->
    Notifications = road_clearance:generate_notifications_for_vehicles_coming_to_junction(short_road_mock_spec(), junction1, mock_passed_evs()),
    SortedNotifications = lists:sort(fun(N1, N2) -> compare_notifications(N1, N2) end, Notifications),
    SortedExpected = lists:sort(fun(N1, N2) -> compare_notifications(N1, N2) end, notifications_from_junction_list()),
    Pairs = lists:zip(SortedExpected, SortedNotifications),
    lists:foreach(fun({Expected, Got}) -> ?assertEqual(Expected, Got) end, Pairs).


generate_info_for_junctions_test() ->
    InfoForJunctions = road_clearance:generate_info_for_junctions(mock_road_spec(), mock_evs()),
    ?assertEqual(mock_info_for_junctions(), InfoForJunctions).


%% internal functions

compare_notifications(#{ride_id := FirstRideId, notification_code := FirstCode}, #{ride_id := SecondRideId, notification_code := SecondCode}) ->
    case FirstRideId == SecondRideId of
        true -> FirstCode =< SecondCode;
        false -> FirstRideId < SecondRideId
    end.


mock_road_spec() ->
    #{
        id => mock_road,
        length => 200.0,
        beginning_at => junction1,
        ending_at => junction2
    }.


mock_evs() ->
    #{
        first_ride => #{
                part_of_road => 0.0,
                direction => forward,
                emergency_service_type => mock_service_type
            },
        second_ride => #{
                part_of_road => 1.0,
                direction => backward,
                emergency_service_type => mock_service_type
            },
        third_ride => #{
                part_of_road => 0.3,
                direction => forward,
                emergency_service_type => mock_service_type
            },
        fourth_ride => #{
                part_of_road => 0.4,
                direction => backward,
                emergency_service_type => mock_service_type
            },
        fifth_ride => #{
                part_of_road => 0.8,
                direction => forward,
                emergency_service_type => mock_service_type
            }
    }.


basic_notifications_list() ->
    [
        #{
            ride_id => first_ride,
            road_network_element_type => road,
            road_network_element_id => mock_road,
            beginning_at => 0.0,
            ending_at => 0.5,
            direction => forward,
            notification_code => notification_codes:make_way_on_road_code()
        },
        #{
            ride_id => second_ride,
            road_network_element_type => road,
            road_network_element_id => mock_road,
            beginning_at => 0.5,
            ending_at => 1.0,
            direction => backward,
            notification_code => notification_codes:make_way_on_road_code()
        },
        #{
            ride_id => third_ride,
            road_network_element_type => road,
            road_network_element_id => mock_road,
            beginning_at => 0.3,
            ending_at => 0.8,
            direction => forward,
            notification_code => notification_codes:make_way_on_road_code()
        },
        #{
            ride_id => fourth_ride,
            road_network_element_type => road,
            road_network_element_id => mock_road,
            beginning_at => 0.0,
            ending_at => 0.4,
            direction => backward,
            notification_code => notification_codes:make_way_on_road_code()
        },
        #{
            ride_id => fifth_ride,
            road_network_element_type => road,
            road_network_element_id => mock_road,
            beginning_at => 0.8,
            ending_at => 1.0,
            direction => forward,
            notification_code => notification_codes:make_way_on_road_code()
        },
        #{
            ride_id => first_ride,
            road_network_element_type => road,
            road_network_element_id => mock_road,
            beginning_at => 0.0,
            ending_at => 0.5,
            direction => backward,
            notification_code => notification_codes:action_unnecessary_code()
        },
        #{
            ride_id => second_ride,
            road_network_element_type => road,
            road_network_element_id => mock_road,
            beginning_at => 0.5,
            ending_at => 1.0,
            direction => forward,
            notification_code => notification_codes:action_unnecessary_code()
        },
        #{
            ride_id => third_ride,
            road_network_element_type => road,
            road_network_element_id => mock_road,
            beginning_at => 0.3,
            ending_at => 0.8,
            direction => backward,
            notification_code => notification_codes:action_unnecessary_code()
        },
        #{
            ride_id => fourth_ride,
            road_network_element_type => road,
            road_network_element_id => mock_road,
            beginning_at => 0.0,
            ending_at => 0.4,
            direction => forward,
            notification_code => notification_codes:action_unnecessary_code()
        },
        #{
            ride_id => fifth_ride,
            road_network_element_type => road,
            road_network_element_id => mock_road,
            beginning_at => 0.8,
            ending_at => 1.0,
            direction => backward,
            notification_code => notification_codes:action_unnecessary_code()
        }
    ].


short_road_mock_spec() ->
    #{
        id => mock_road,
        length => 50.0,
        beginning_at => junction1,
        ending_at => junction2
    }.


mock_passed_evs() ->
    #{
        first_ride => #{
                part_of_road => 0.7,
                direction => forward,
                emergency_service_type => mock_service_type,
                remaining_distance => 60.0
            },
        second_ride => #{
                part_of_road => 0.4,
                direction => backward,
                emergency_service_type => mock_service_type,
                remaining_distance => 10.0
            }
    }.


notifications_from_junction_list() ->
    [
        #{
            ride_id => first_ride,
            road_network_element_type => road,
            road_network_element_id => mock_road,
            beginning_at => 0.0,
            ending_at => 1.0,
            direction => backward,
            notification_code => notification_codes:make_way_on_junction_code()
        },
        #{
            ride_id => second_ride,
            road_network_element_type => road,
            road_network_element_id => mock_road,
            beginning_at => 0.0,
            ending_at => 0.2,
            direction => backward,
            notification_code => notification_codes:make_way_on_junction_code()
        }
    ].


mock_info_for_junctions() ->
    [
        #{
            junction_id => junction2,
            road => mock_road,
            evs => #{
                    fifth_ride => #{
                        remaining_distance => 60.0,
                        emergency_service_type => mock_service_type
                    }
                }
        },
        #{
            junction_id => junction1,
            road => mock_road,
            evs => #{
                    fourth_ride => #{
                        remaining_distance => 20.0,
                        emergency_service_type => mock_service_type
                    }
                }
        }
    ].