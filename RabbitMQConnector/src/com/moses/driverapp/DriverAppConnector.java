package com.moses.driverapp;

import com.moses.Position;
import com.moses.RabbitMqConnector;
import com.moses.marshalling.NotificationMarshaller;
import com.moses.marshalling.PositionMarshaller;
import com.rabbitmq.client.BuiltinExchangeType;

import java.io.IOException;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;
import java.util.function.Consumer;

public class  DriverAppConnector {
    private final static String exchangeName = "moses_exchange";
    private final static BuiltinExchangeType exchangeType = BuiltinExchangeType.DIRECT;

    private String routingKey = "mock_road";
    private final static String localizationUpdateRoutingKey = "localization_update";

    private final String notificationQueue;

    private final static String host = "localhost";

    private final RabbitMqConnector connector;

    public DriverAppConnector() throws IOException, TimeoutException {
        connector = new RabbitMqConnector(host);
        connector.setupExchange(exchangeName, exchangeType);
        notificationQueue = connector.setupQueue(exchangeName, routingKey);
    }

    public void listenForNotifications(Consumer<Notification> userCallback) throws IOException {
        connector.listenForMessages(notificationQueue, rawNotification ->  userCallback.accept(NotificationMarshaller.unmarshall(rawNotification)));
    }

    public void sendLocalization(double longitude, double latitude) throws IOException {
        byte[] messageBody = PositionMarshaller.marshallLocalizationUpdate(latitude, longitude);
        connector.sendMessage(exchangeName,
                localizationUpdateRoutingKey,
                messageBody);
    }

    public Position updateLocalization(double longitude, double latitude) throws IOException, InterruptedException {
        byte[] requestBody = PositionMarshaller.marshallLocalizationUpdate(latitude, longitude);
        byte[] response = connector.performRPC(exchangeName,
                localizationUpdateRoutingKey,
                requestBody);
        return PositionMarshaller.unmarshallPosition(response);
    }
}
