-module(end_to_end_test_dispatch).

-export([establish_connection/0, get_new_rides/1]).

establish_connection() ->
    {ok, {mock_connection, ride_not_started_yet}}.

get_new_rides({mock_connection, ride_not_started_yet}) ->
    {ok, [mock_ride_data], {mock_connection, ride_already_started}};

get_new_rides({mock_connection, ride_alread_started} = Connection) ->
    {ok, [], Connection}.

