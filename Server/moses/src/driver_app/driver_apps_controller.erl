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
    io:format("Received message: ~s~n", [rabbitmq_connector:get_message_body(Message)]),
    Position = update_position(Message),
    reply_with_position(Channel, Message, Position),
    {noreply, {Connection, Channel}}.

terminate(_, _, {Connection, Channel}) ->
    rabbitmq_connector:delete_queue(queue_name(), Channel, Connection).

%% internal functions

update_position(Message) ->
    MessageBody = rabbitmq_connector:get_message_body(Message),
    {mock_road, 0.5}.

reply_with_position(Channel, Message, Position) ->
    ReplyBody = marshallPosition(Position),
    rabbitmq_connector:send_reply(Channel, Message, ReplyBody).

marshallPosition({RoadId, PartOfRoad}) ->
    list_to_binary([trunc(PartOfRoad * 100), atom_to_binary(RoadId, encoding())]).