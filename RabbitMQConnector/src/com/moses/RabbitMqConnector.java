package com.moses;

import com.rabbitmq.client.*;
import com.rabbitmq.client.AMQP.BasicProperties;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.*;
import java.util.function.BiConsumer;


public class RabbitMqConnector {
    private final Connection connection;
    private final Map<Thread, Channel> channels;

    private String callbackQueue;

    public RabbitMqConnector(String host, String username, String password) throws IOException, TimeoutException {
        ConnectionFactory factory = new ConnectionFactory();
        factory.setHost(host);
        factory.setUsername(username);
        factory.setPassword(password);
        connection = factory.newConnection();
        channels = new HashMap<>();
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
        setupCallbackQueue(channel);
        BasicProperties properties = preparePropertiesForRPC(callbackQueue);
        CompletableFuture.runAsync(() -> {
            try {
                sendMessageWithProperties(exchangeName, routingKey, requestBody, properties);
            } catch (IOException e) {
                e.printStackTrace();
            }
        });
        final BlockingQueue<byte[]> resultBuffer = new ArrayBlockingQueue<>(1);
        String ctag = channel.basicConsume(callbackQueue, true, (consumerTag, delivery) -> {
            resultBuffer.offer(delivery.getBody());
        }, consumerTag -> {});
        byte[] result = resultBuffer.take();
//        channel.basicCancel(ctag);
        return result;
    }

    private Channel getChannel() throws IOException {
        if (!channels.containsKey(Thread.currentThread())) {
            Channel channel = connection.createChannel();
            channels.put(Thread.currentThread(), channel);
        }
        return channels.get(Thread.currentThread());
    }

    private void sendMessageWithProperties(String exchangeName, String routingKey, byte[] messageBody, BasicProperties properties) throws IOException {
        Channel channel = getChannel();
        channel.basicPublish(exchangeName, routingKey, properties, messageBody);
    }

    private void setupCallbackQueue(Channel channel) throws IOException {
        if (callbackQueue == null) {
            callbackQueue = channel.queueDeclare().getQueue();
        }
    }

    private BasicProperties preparePropertiesForRPC(String callbackQueue) {
        return new BasicProperties
                .Builder()
                .replyTo(callbackQueue)
                .build();
    }
}
