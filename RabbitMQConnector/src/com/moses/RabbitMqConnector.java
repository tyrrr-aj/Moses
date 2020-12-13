package com.moses;

import com.moses.driverapp.backend.synchronization.SyncedObject;
import com.rabbitmq.client.*;
import com.rabbitmq.client.AMQP.BasicProperties;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.*;
import java.util.function.BiConsumer;


public class RabbitMqConnector {
    private final Connection connection;
    private final SyncedObject<Map<Thread, Channel>> channels;
    private final SyncedObject<Map<Thread, BlockingQueue<byte[]>>> resultBuffers;
    private final SyncedObject<Map<Thread, String>> callbackQueues;

    public RabbitMqConnector(String host, String username, String password) throws IOException, TimeoutException {
        ConnectionFactory factory = new ConnectionFactory();
        factory.setHost(host);
        factory.setUsername(username);
        factory.setPassword(password);
        connection = factory.newConnection();
        channels = new SyncedObject<>(new HashMap<>());
        resultBuffers = new SyncedObject<>(new HashMap<>());
        callbackQueues = new SyncedObject<>(new HashMap<>());
    }

    public void setupExchange(String name, BuiltinExchangeType type) throws IOException {
        Channel channel = getChannel();
        channel.exchangeDeclare(name, type);
    }

    public String setupAndBindQueue(String exchangeName, String routingKey) throws IOException {
        Channel channel = getChannel();
        String queue = channel.queueDeclare().getQueue();
        channel.queueBind(queue, exchangeName, routingKey);

        return queue;
    }

    public String setupQueue() throws IOException {
        Channel channel = getChannel();
        return channel.queueDeclare().getQueue();
    }

    public String setupQueue(String name) throws IOException {
        Channel channel = getChannel();
        return channel.queueDeclare(name, false, true, true, null).getQueue();
    }

    public void bindQueue(String queue, String exchangeName, String routingKey) throws IOException {
        Channel channel = getChannel();
        channel.queueBind(queue, exchangeName, routingKey);
    }

    public void unbindQueue(String queue, String exchangeName, String routingKey) throws IOException {
        Channel channel = getChannel();
        channel.queueUnbind(queue, exchangeName, routingKey);
    }

    public String listenForMessages(String queue, BiConsumer<byte[], String> userCallback) throws IOException {
        DeliverCallback deliverCallback = (consumerTag, delivery) -> {
            userCallback.accept(delivery.getBody(), delivery.getEnvelope().getRoutingKey());
        };

        Channel listeningChannel = getChannel();

        return listeningChannel.basicConsume(queue, true, deliverCallback, consumerTag -> {});
    }

    public void stopListening(String consumerTag) throws IOException, TimeoutException {
        Channel channel = getChannel();
        channel.basicCancel(consumerTag);
    }

    public void closeChannel() throws IOException, TimeoutException {
        Channel channel = getChannel();
        channel.close();
    }

    public void closeConnection() throws IOException {
        connection.close();
    }

    public void sendMessage(String exchangeName, String routingKey, byte[] body) throws IOException {
        sendMessageWithProperties(exchangeName, routingKey, body, null);
    }

    public byte[] performRPC(String exchangeName, String routingKey, byte[] requestBody) throws IOException, InterruptedException {
        Channel channel = getChannel();
        String callbackQueue = getCallbackQueue(channel);
        BlockingQueue<byte[]> resultBuffer = setupListening(channel, callbackQueue);
        BasicProperties properties = preparePropertiesForRPC(callbackQueue);
        CompletableFuture.runAsync(() -> {
            try {
                sendMessageWithProperties(exchangeName, routingKey, requestBody, properties);
            } catch (IOException e) {
                e.printStackTrace();
            }
        });
        return resultBuffer.take();
    }

    private void sendMessageWithProperties(String exchangeName, String routingKey, byte[] messageBody, BasicProperties properties) throws IOException {
        Channel channel = getChannel();
        channel.basicPublish(exchangeName, routingKey, properties, messageBody);
    }

    private Channel getChannel() throws IOException {
        if (!channels.applyAndGet((channelMap) -> channelMap.containsKey(Thread.currentThread()))) {
            Channel channel = connection.createChannel();
            channels.update((channelMap) -> {channelMap.put(Thread.currentThread(), channel);});
        }
        return channels.applyAndGet((channelMap) -> channelMap.get(Thread.currentThread()));
    }

    private String getCallbackQueue(Channel channel) throws IOException {
        if (!callbackQueues.applyAndGet((queueMap) -> queueMap.containsKey(Thread.currentThread()))) {
            String callbackQueue = channel.queueDeclare().getQueue();
            callbackQueues.update((queueMap) -> {queueMap.put(Thread.currentThread(), callbackQueue);});
        }
        return callbackQueues.applyAndGet((queueMap) -> queueMap.get(Thread.currentThread()));
    }

    private BlockingQueue<byte[]> setupListening(Channel channel, String callbackQueue) throws IOException {
        if (!resultBuffers.applyAndGet((bufferMap) -> bufferMap.containsKey(Thread.currentThread()))) {
            BlockingQueue<byte[]> buffer = new ArrayBlockingQueue<>(1);
            resultBuffers.update((bufferMap) -> {bufferMap.put(Thread.currentThread(), buffer);});

            channel.basicConsume(callbackQueue, true, (consumerTag, delivery) -> {
                buffer.offer(delivery.getBody());
            }, consumerTag -> {});
        }

        return resultBuffers.applyAndGet((bufferMap) -> bufferMap.get(Thread.currentThread()));
    }

    private BasicProperties preparePropertiesForRPC(String callbackQueue) {
        return new BasicProperties
                .Builder()
                .replyTo(callbackQueue)
                .build();
    }
}
