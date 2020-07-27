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
    
    close(Channel, Connection).

setup_exchange_test() ->
    {ok, Connection} = notifier:setup_connection(),
    {ok, Channel} = notifier:get_channel(Connection),
    
    Declare = #'exchange.declare'{exchange = notifier:exchange_name(), passive = true},
    ?assertEqual(#'exchange.declare_ok'{}, amqp_channel:call(Channel, Declare)),
    
    close(Channel, Connection).

notify_test() ->
    {ok, Connection} = notifier:setup_connection(),
    {ok, Channel} = notifier:get_channel(Connection),
    prepare_test_queue(Channel),

    notifier:notify(mock_notification(), Channel),

    ?assertEqual(notifier:marshall_notification(mock_notification()), receive_single_notification()),

    delete_test_queue(Channel),
    close(Channel, Connection).

%% constants

subscription_ack_timeout() ->
    1000.

receive_message_timeout() ->
    1000.

test_queue_name() ->
    <<"mock_queue">>.

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

%% internal functions

close(Channel, Connection) ->
    amqp_channel:close(Channel),
    amqp_connection:close(Connection).

prepare_test_queue(Channel) ->
    declare_test_queue(Channel),
    bind_test_queue(Channel),
    subscribe_on_test_queue(Channel).

declare_test_queue(Channel) ->
    Declare = #'queue.declare'{queue = test_queue_name()},
    #'queue.declare_ok'{} = amqp_channel:call(Channel, Declare).

bind_test_queue(Channel) ->
    Binding = #'queue.bind'{queue       = test_queue_name(),
                            exchange    = notifier:exchange_name(),
                            routing_key = atom_to_binary(mock_road_id(), notifier:routing_key_encoding())},
    #'queue.bind_ok'{} = amqp_channel:call(Channel, Binding).

subscribe_on_test_queue(Channel) ->
    #'basic.consume_ok'{} = amqp_channel:call(Channel, #'basic.consume'{queue = test_queue_name(), no_ack = true}),
    ok = receive_subscription_ack().

receive_subscription_ack() ->
    receive
        #'basic.consume_ok'{} ->
            ok
    after subscription_ack_timeout() ->
        {error, subscription_ack_timeout}
    end.

receive_single_notification() ->
    receive
        {#'basic.deliver'{}, {_, _, MessageBody}} ->
            MessageBody
    after receive_message_timeout() ->
        {error, receive_message_timeout}
    end.

delete_test_queue(Channel) ->
    Delete = #'queue.delete'{queue = test_queue_name()},
    #'queue.delete_ok'{} = amqp_channel:call(Channel, Delete).