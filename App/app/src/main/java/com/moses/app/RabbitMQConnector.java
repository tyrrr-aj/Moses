package com.moses.app;

import android.os.Build;

import androidx.annotation.RequiresApi;

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
    }

    @RequiresApi(api = Build.VERSION_CODES.N)
    public void listenForNotifications(Consumer<byte[]> userCallback) throws IOException {
        DeliverCallback deliverCallback = (consumerTag, delivery) -> {
            userCallback.accept(delivery.getBody());
        };

        channel.basicConsume(notificationQueue, true, deliverCallback, consumerTag -> {});
    }

    public void sendLocalization(double longitude, double latitude) throws IOException {
        Marshaller marshaller = new Marshaller();
        channel.basicPublish(exchangeName,
                localizationUpdateRoutingKey,
                null,
                marshaller.marshallLocalizationUpdate(latitude, longitude, notificationQueue));
    }
}
