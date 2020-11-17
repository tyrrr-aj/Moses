-module(driver_apps_controller).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/3]).
-export([start_link/0]).

%% constants

queue_name() -> <<"localization_requests">>.
routing_key() -> <<"localization_update">>.
timeout() -> 1000.
encoding() -> latin1.

%% interface

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% callbacks

init(_) ->
    {ok, Connection, Channel} = rabbitmq_connector:setup_connection_and_channel(),
    ok = rabbitmq_connector:prepare_queue(Channel, queue_name(), routing_key(), timeout()),
    {ok, {Connection, Channel}}.

handle_call(_, _, _) ->
    unimplemented.

handle_cast(_, _) ->
    unimplemented.

handle_info(Message, {Connection, Channel}) ->
    % io:format("Received message\: ~s~n", [rabbitmq_connector:get_message_body(Message)]),
    Position = update_position(Message),
    % io:format("Position: ~p~n", [Position]),
    reply_with_position(Channel, Message, Position),
    {noreply, {Connection, Channel}}.

terminate(_, _, {Connection, Channel}) ->
    rabbitmq_connector:delete_queue(queue_name(), Channel, Connection).

%% internal functions

update_position(Message) ->
    MessageBody = rabbitmq_connector:get_message_body(Message),
    GPSCoords = parse_coords(MessageBody),
    map_controller:get_position(GPSCoords).


reply_with_position(Channel, Message, Position) ->
    ReplyBody = marshall_position(Position),
    rabbitmq_connector:send_reply(Channel, Message, ReplyBody).


marshall_position(unknown) ->
    atom_to_binary(unknown, encoding());

marshall_position({road, RoadId, PartOfRoad}) ->
    list_to_binary([trunc(PartOfRoad * 100), atom_to_binary(RoadId, encoding())]);

marshall_position({junction, JunctionId}) ->
    list_to_binary([atom_to_binary(junction, encoding()), atom_to_binary(JunctionId, encoding())]).


parse_coords(MessageBody) ->
    RawBody = binary_to_list(MessageBody),
    #{
        lon => coord(lists:sublist(RawBody, 7)),
        lat => coord(lists:sublist(RawBody, 8, 14))
    }.

coord(ByteRepr) ->
    list_to_integer(ByteRepr) / 100000.0.