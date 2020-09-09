-module(rabbitmq_con).

-include_lib("amqp_client/include/amqp_client.hrl").

-export([prepare_test_queue/4, delete_test_queue/2, receive_single_notification/1, close/2]).

close(Channel, Connection) ->
    amqp_channel:close(Channel),
    amqp_connection:close(Connection).

prepare_test_queue(Channel, QueueName, RoutingKey, Timeout) ->
    declare_test_queue(Channel, QueueName),
    bind_test_queue(Channel, QueueName, RoutingKey),
    subscribe_on_test_queue(Channel, QueueName, Timeout).

declare_test_queue(Channel, QueueName) ->
    Declare = #'queue.declare'{queue = QueueName},
    #'queue.declare_ok'{} = amqp_channel:call(Channel, Declare).

bind_test_queue(Channel, QueueName, RoutingKey) ->
    Binding = #'queue.bind'{queue       = QueueName,
                            exchange    = notifier:exchange_name(),
                            routing_key = RoutingKey},
    #'queue.bind_ok'{} = amqp_channel:call(Channel, Binding).

subscribe_on_test_queue(Channel, QueueName, Timeout) ->
    #'basic.consume_ok'{} = amqp_channel:call(Channel, #'basic.consume'{queue = QueueName, no_ack = true}),
    ok = receive_subscription_ack(Timeout).

receive_subscription_ack(Timeout) ->
    receive
        #'basic.consume_ok'{} ->
            ok
    after Timeout ->
        {error, subscription_ack_timeout}
    end.

receive_single_notification(Timeout) ->
    receive
        {#'basic.deliver'{}, {_, _, MessageBody}} ->
            MessageBody
    after Timeout ->
        {error, receive_message_timeout}
    end.

delete_test_queue(Channel, QueueName) ->
    Delete = #'queue.delete'{queue = QueueName},
    #'queue.delete_ok'{} = amqp_channel:call(Channel, Delete).