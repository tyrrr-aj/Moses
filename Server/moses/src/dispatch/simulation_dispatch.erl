-module(simulation_dispatch).

-behaviour(gen_server).

-export([establish_connection/0, get_new_rides/1, get_ended_rides/1]). % dispatch API
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]). % gen_server callbacks

-define(SERVER, ?MODULE).


%% dispatch API

establish_connection() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


get_new_rides(DispatchPid) ->
    NewRides = gen_server:call(DispatchPid, new_rides),
    {ok, NewRides}.


get_ended_rides(DispatchPid) ->
    EndedRides = gen_server:call(DispatchPid, ended_rides),
    {ok, EndedRides}.


%% Callbacks

init([]) ->
    {ok, Connection, Channel} = rabbitmq_connector:setup_simulation_connection_and_channel(),
    ok = rabbitmq_connector:prepare_queue(Channel, queue_name(), routing_key(), timeout(), simulation),
    {ok, {{Connection, Channel}, [], []}}.


handle_call(new_rides, _From, {ConnDetails, PendingRides, EndedRides}) ->
    {reply, PendingRides, {ConnDetails, [], EndedRides}};

handle_call(ended_rides, _From, {ConnDetails, PendingRides, EndedRides}) ->
    {reply, EndedRides, {ConnDetails, PendingRides, []}}.


handle_cast(_, _State) ->
    should_not_be_used.


handle_info(Message, {ConnDetails, PendingRides, EndedRides}) ->
    MessageBody = rabbitmq_connector:get_message_body(Message),
    case ride_info(parse_message(MessageBody)) of
        {end_ride, EndedRide} -> {noreply, {ConnDetails, PendingRides, EndedRides ++ [EndedRide]}};
        {new_ride, NewRide} -> {noreply, {ConnDetails, PendingRides ++ [NewRide], EndedRides}}
    end.


%% Internal functions

ride_info({end_ride, VehicleId}) ->
    {end_ride, VehicleId};

ride_info({new_ride, #{vehicle_id := RideId}}) ->
    {new_ride,
    #{
        ride_id => RideId,
        emergency_service_type => simulation
    }}.


parse_message(MessageBody) ->
    RawBody = binary_to_list(MessageBody),
    case lists:split(8, RawBody) of
        {"end_ride", VehicleId} -> {end_ride, VehicleId};
        _ ->
            {Coords, VehicleId} = lists:split(14, RawBody),
            {new_ride, 
                #{
                    lon => coord(lists:sublist(Coords, 7)),
                    lat => coord(lists:sublist(Coords, 8, 14)),
                    vehicle_id => VehicleId
                }}
    end.


coord(ByteRepr) ->
    list_to_integer(ByteRepr) / 100000.0.


%% internal constants

queue_name() -> <<"dispatch_queue">>.


routing_key() -> <<"dispatch.*">>.


timeout() -> 1000.