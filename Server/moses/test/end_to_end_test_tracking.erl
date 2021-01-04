-module(end_to_end_test_tracking).

-export([start_tracking/1, get_current_position/2, stop_tracking/1]).

start_tracking(mock_ride_data) ->
    {ok, mock_tracking_info()}.

get_current_position(mock_ride, mock_connection) ->
    {ok, mock_gps_coords}.

stop_tracking(#{ride_id := mock_ride}) ->
    ok.

%% internal functions

mock_tracking_info() ->
    #{
        current_position => {road, mock_road, 0.5},
        previous_position => {road, mock_road, 0.4},
        direction => forward,
        ride_id => mock_ride,
        emergency_service_type => end_to_end_test,
        connection => mock_connection
    }.