-module(notifier_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

setup_connection_test() ->
    {ResponseCode, Connection} = notifier:setup_connection(),

    ?assertEqual(ok, ResponseCode),

    amqp_connection:close(Connection).

get_channel_test() ->
    {ok, Connection} = notifier:setup_connection(),

    {ResponseCode, Channel} = notifier:get_channel(Connection),

    ?assertEqual(ok, ResponseCode),
    
    rabbitmq_con:close(Channel, Connection).

setup_exchange_test() ->
    {ok, Connection} = notifier:setup_connection(),
    {ok, Channel} = notifier:get_channel(Connection),
    
    Declare = #'exchange.declare'{exchange = notifier:exchange_name(), passive = true},
    ?assertEqual(#'exchange.declare_ok'{}, amqp_channel:call(Channel, Declare)),
    
    rabbitmq_con:close(Channel, Connection).

notify_test() ->
    {ok, Connection} = notifier:setup_connection(),
    {ok, Channel} = notifier:get_channel(Connection),
    rabbitmq_con:prepare_test_queue(Channel, test_queue_name(), test_routing_key(), subscription_ack_timeout()),

    notifier:notify(mock_notification(), Channel),

    ?assertEqual(notifier:marshall_notification(mock_notification()), rabbitmq_con:receive_single_notification(receive_message_timeout())),

    rabbitmq_con:delete_test_queue(Channel, test_queue_name()),
    rabbitmq_con:close(Channel, Connection).

%% constants

subscription_ack_timeout() ->
    1000.

receive_message_timeout() ->
    1000.

test_queue_name() ->
    <<"mock_queue">>.

test_routing_key() ->
    atom_to_binary(mock_road_id(), notifier:routing_key_encoding()).

mock_road_id() ->
    mock_road.

mock_notification() ->
    #{
        road_id => mock_road_id(),
        begining_at => 0.0,
        ending_at => 1.0,
        direction => forward,
        notification_body => <<"mock notification">>
    }.
