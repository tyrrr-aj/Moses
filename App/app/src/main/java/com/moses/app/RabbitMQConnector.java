package com.moses.app;

import android.os.Build;

import androidx.annotation.RequiresApi;

import com.rabbitmq.client.AMQP.BasicProperties;
import com.rabbitmq.client.BuiltinExchangeType;
import com.rabbitmq.client.Channel;
import com.rabbitmq.client.Connection;
import com.rabbitmq.client.ConnectionFactory;
import com.rabbitmq.client.DeliverCallback;

import java.io.IOException;
import java.util.concurrent.TimeoutException;
import java.util.function.Consumer;

public class RabbitMQConnector {
    private final static String exchangeName = "moses_exchange";

    private ConnectionFactory factory;
    private Connection connection;
    private Channel channel;

    private String notificationQueue;
    private String routingKey = "mock_road";
    private final static String localizationUpdateRoutingKey = "localization_update";

    private String localizationQueue;

    public RabbitMQConnector() {
        factory = new ConnectionFactory();
        factory.setHost("10.0.2.2");
    }

    public void connect() throws IOException, TimeoutException {
        connection = factory.newConnection();
        channel = connection.createChannel();

        channel.exchangeDeclare(exchangeName, BuiltinExchangeType.DIRECT);

        notificationQueue = channel.queueDeclare().getQueue();
        channel.queueBind(notificationQueue, exchangeName, routingKey);

        localizationQueue = channel.queueDeclare().getQueue();
    }

    @RequiresApi(api = Build.VERSION_CODES.N)
    public void listenForNotifications(Consumer<byte[]> userCallback) throws IOException {
        DeliverCallback deliverCallback = (consumerTag, delivery) -> {
            userCallback.accept(delivery.getBody());
        };

        channel.basicConsume(notificationQueue, true, deliverCallback, consumerTag -> {});
    }

    @RequiresApi(api = Build.VERSION_CODES.N)
    public void listenForLocalization(Consumer<byte[]> userCallback) throws IOException {
        DeliverCallback deliverCallback = (consumerTag, delivery) -> {
            userCallback.accept(delivery.getBody());
        };

        channel.basicConsume(localizationQueue, true, deliverCallback, consumerTag -> {});
    }

    public void sendLocalization(double longitude, double latitude) throws IOException {
        BasicProperties properties = new BasicProperties
                .Builder()
                .replyTo(localizationQueue)
                .build();

        Marshaller marshaller = new Marshaller();
        channel.basicPublish(exchangeName,
                localizationUpdateRoutingKey,
                properties,
                marshaller.marshallLocalizationUpdate(latitude, longitude, notificationQueue));
    }
}
