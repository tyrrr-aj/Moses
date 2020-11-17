-module(simulation_tracking).

-behaviour(gen_server).

-export([start_tracking/1, get_current_coords/1, stop_tracking/1]).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(SERVER, ?MODULE).


start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


%% tracking API

start_tracking(RideId) ->
    Coords = gen_server:call(?SERVER, {add_ride, RideId}, infinity),
    {ok, Coords}.

get_current_coords(RideId) ->
    Coords = gen_server:call(?SERVER, {get_position, RideId}),
    {ok, Coords}.

stop_tracking(_RideId) ->
    ok.


%% Callbacks

init([]) ->
    RabbitMqConn = setup_rabbitmq(),
    {ok, #{rabbitmq_conn => RabbitMqConn, coords => #{}}}.

handle_call({get_position, RideId}, _From, #{coords := CoordsMap}=State) ->
    % io:format("CoordsMap: ~p~n", [CoordsMap]),
    {reply, get_coords(RideId, CoordsMap), State};

handle_call({add_ride, RideId}, _From, #{coords := CoordsMap}=State) ->
    try get_coords(RideId, CoordsMap) of
        Coords -> {reply, Coords, State}
    catch
        error:{badkey, _} -> {reply, unknown, State#{coords => CoordsMap#{RideId => unknown}}}
    end.

handle_cast(_, _State) ->
    should_not_be_used.

handle_info(Message, #{coords := Coords}=State) ->
    RawBody = rabbitmq_connector:get_message_body(Message),
    % io:format("Received message: ~s~n", [RawBody]),
    case parse_message(RawBody) of
        {RideId, end_ride} -> {noreply, State#{coords => maps:remove(RideId, Coords)}};
        {RideId, Coords} -> {noreply, State#{coords => Coords#{RideId => Coords}}}
    end.

terminate(_, #{rabbitmq_conn := {Channel, Connection}}) ->
    rabbitmq_connector:close(queue_name(), Channel, Connection).

%% Internal functions

setup_rabbitmq() ->
    {ok, Connection, Channel} = rabbitmq_connector:setup_simulation_connection_and_channel(),
    ok = rabbitmq_connector:prepare_queue(Channel, queue_name(), routing_key(), timeout(), simulation),
    {Connection, Channel}.

get_coords(RideId, Coords) ->
    maps:get(RideId, Coords).


%% internal functions

parse_message(MessageBody) ->
    RawBody = binary_to_list(MessageBody),
    case lists:sublist(RawBody, 8) of
        "end_ride" -> 
            {_, VehicleId} = lists:split(RawBody, 8),
            {VehicleId, end_ride};
        _ ->
            {Coords, VehicleId} = lists:split(14, RawBody),
            {VehicleId, #{
                lon => coord(lists:sublist(Coords, 7)),
                lat => coord(lists:sublist(Coords, 8, 14))
            }}
    end.

coord(ByteRepr) ->
    list_to_integer(ByteRepr) / 100000.0.


%% internal constants

queue_name() -> <<"tracking_queue">>.

routing_key() -> <<"tracking.*">>.

timeout() -> 1000.