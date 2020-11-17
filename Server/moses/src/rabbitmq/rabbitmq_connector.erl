-module(rabbitmq_connector).

-include_lib("amqp_client/include/amqp_client.hrl").

-export([setup_connection_and_channel/0, setup_simulation_connection_and_channel/0, prepare_queue/4, prepare_queue/5, receive_single_notification/1, send_reply/3, get_message_body/1, get_callback_routing_key/1, close/3]).

exchange_name() -> <<"moses_exchange">>.
simulation_exchange_name() -> <<"moses_simulation_exchange">>.


setup_connection_and_channel() ->
    {ok, Connection} = amqp_connection:start(#amqp_params_network{}),
    {ok, Channel} = amqp_connection:open_channel(Connection),
    ok = setup_exchange(Channel, exchange_name()),
    {ok, Connection, Channel}.

setup_simulation_connection_and_channel() ->
    {ok, Connection} = amqp_connection:start(#amqp_params_network{}),
    {ok, Channel} = amqp_connection:open_channel(Connection),
    ok = setup_simulation_exchange(Channel, simulation_exchange_name()),
    {ok, Connection, Channel}.

close(QueueName, Channel, Connection) ->
    delete_queue(Channel, QueueName),
    amqp_channel:close(Channel),
    amqp_connection:close(Connection).

prepare_queue(Channel, QueueName, RoutingKey, Timeout) ->
    declare_queue(Channel, QueueName),
    bind_queue(Channel, QueueName, RoutingKey),
    subscribe_on_queue(Channel, QueueName, Timeout),
    ok.

prepare_queue(Channel, QueueName, RoutingKey, Timeout, simulation) ->
    declare_queue(Channel, QueueName),
    bind_queue(Channel, QueueName, RoutingKey, simulation_exchange_name()),
    subscribe_on_queue(Channel, QueueName, Timeout),
    ok.

declare_queue(Channel, QueueName) ->
    Declare = #'queue.declare'{queue = QueueName},
    #'queue.declare_ok'{} = amqp_channel:call(Channel, Declare).

bind_queue(Channel, QueueName, RoutingKey) ->
    Binding = #'queue.bind'{queue       = QueueName,
                            exchange    = exchange_name(),
                            routing_key = RoutingKey},
    #'queue.bind_ok'{} = amqp_channel:call(Channel, Binding).

bind_queue(Channel, QueueName, RoutingKey, ExchangeName) ->
    Binding = #'queue.bind'{queue       = QueueName,
                            exchange    = ExchangeName,
                            routing_key = RoutingKey},
    #'queue.bind_ok'{} = amqp_channel:call(Channel, Binding).

subscribe_on_queue(Channel, QueueName, Timeout) ->
    #'basic.consume_ok'{} = amqp_channel:call(Channel, #'basic.consume'{queue = QueueName, no_ack = true}),
    ok = receive_subscription_ack(Timeout).

send_reply(Channel, OriginalMessage, ReplyBody) ->
    RoutingKey = get_callback_routing_key(OriginalMessage),
    Publish = #'basic.publish'{exchange = <<"">>, routing_key = RoutingKey},
    amqp_channel:cast(Channel, Publish, #amqp_msg{payload = ReplyBody}).
    % io:format("Replied to: ~s~n", [RoutingKey]).

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

get_message_body({#'basic.deliver'{}, #amqp_msg{payload = MessageBody}}) ->
    MessageBody.

get_callback_routing_key({#'basic.deliver'{}, #amqp_msg{props = Properties}}) ->
    #'P_basic'{reply_to = CallbackQueue} = Properties,
    CallbackQueue.

delete_queue(Channel, QueueName) ->
    Delete = #'queue.delete'{queue = QueueName},
    #'queue.delete_ok'{} = amqp_channel:call(Channel, Delete).


%% private functions

setup_simulation_exchange(Channel, ExchangeName) ->
    Declare = #'exchange.declare'{exchange = ExchangeName, type = <<"topic">>},
    #'exchange.declare_ok'{} = amqp_channel:call(Channel, Declare),
    ok.

setup_exchange(Channel, ExchangeName) ->
    Declare = #'exchange.declare'{exchange = ExchangeName},
    #'exchange.declare_ok'{} = amqp_channel:call(Channel, Declare),
    ok.

