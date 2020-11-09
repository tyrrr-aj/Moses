-module(simulation_dispatch).

-behaviour(gen_server).

-export([establish_connection/0, get_new_rides/1]). % dispatch API
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]). % gen_server callbacks

-define(SERVER, ?MODULE).

%% dispatch API

establish_connection() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

get_new_rides(DispatchPid) ->
    NewRides = gen_server:call(DispatchPid, get_rides),
    {ok, NewRides, DispatchPid}.


%% Callbacks

init([]) ->
    {ok, Connection, Channel} = rabbitmq_connector:setup_simulation_connection_and_channel(),
    ok = rabbitmq_connector:prepare_queue(Channel, queue_name(), routing_key(), timeout(), simulation),
    {ok, {{Connection, Channel}, []}}.

handle_call(get_rides, _From, {ConnDetails, PendingRides}) ->
    {reply, PendingRides, {ConnDetails, []}}.

handle_cast({add_ride, NewRide}, {ConnDetails, PendingRides}) ->
    should_not_be_used.

handle_info(Message, {ConnDetails, PendingRides}) ->
    MessageBody = rabbitmq_connector:get_message_body(Message),
    NewRide = ride_info(parse_message(MessageBody)),
    {noreply, {ConnDetails, PendingRides ++ [NewRide]}}.


%% Internal functions

ride_info(#{vehicle_id := RideId}) ->
    #{
        ride_id => RideId,
        emergency_service_type => simulation
    }.

parse_message(MessageBody) ->
    RawBody = binary_to_list(MessageBody),
    {Coords, VehicleId} = lists:split(14, RawBody),
    #{
        lon => coord(lists:sublist(Coords, 7)),
        lat => coord(lists:sublist(Coords, 8, 14)),
        vehicle_id => VehicleId
    }.

coord(ByteRepr) ->
    list_to_integer(ByteRepr) / 100000.0.

%% internal constants

queue_name() -> <<"dispatch_queue">>.

routing_key() -> <<"dispatch.*">>.

timeout() -> 1000.