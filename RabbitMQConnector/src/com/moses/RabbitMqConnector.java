package com.moses;

import com.rabbitmq.client.*;
import com.rabbitmq.client.AMQP.BasicProperties;

import java.io.IOException;
import java.util.concurrent.*;
import java.util.function.Consumer;


public class RabbitMqConnector {
    private Connection connection;
    private Channel channel;

    public RabbitMqConnector(String host) throws IOException, TimeoutException {
        ConnectionFactory factory = new ConnectionFactory();
        factory.setHost(host);
        connection = factory.newConnection();
        channel = connection.createChannel();
    }

    public void setupExchange(String name, BuiltinExchangeType type) throws IOException {
        channel.exchangeDeclare(name, type);
    }

    public String setupQueue(String exchangeName, String routingKey) throws IOException {
        String queue = channel.queueDeclare().getQueue();
        channel.queueBind(queue, exchangeName, routingKey);

        return queue;
    }

    public void listenForMessages(String queue, Consumer<byte[]> userCallback) throws IOException {
        DeliverCallback deliverCallback = (consumerTag, delivery) -> {
            userCallback.accept(delivery.getBody());
        };

        channel.basicConsume(queue, true, deliverCallback, consumerTag -> {});
    }

    public void sendMessage(String exchangeName, String routingKey, byte[] body) throws IOException {
        sendMessageWithProperties(exchangeName, routingKey, body, null);
    }

    public byte[] performRPC(String exchangeName, String routingKey, byte[] requestBody) throws IOException, InterruptedException {
        String callbackQueue = setupCallbackQueue();
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
        channel.basicCancel(ctag);
        return result;
    }

    private void sendMessageWithProperties(String exchangeName, String routingKey, byte[] messageBody, BasicProperties properties) throws IOException {
        channel.basicPublish(exchangeName, routingKey, properties, messageBody);
    }

    private String setupCallbackQueue() throws IOException {
        return channel.queueDeclare().getQueue();
    }

    private BasicProperties preparePropertiesForRPC(String callbackQueue) {
        return new BasicProperties
                .Builder()
                .replyTo(callbackQueue)
                .build();
    }
}
