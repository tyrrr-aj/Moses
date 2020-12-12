package com.moses.driverapp.backend;

import com.moses.driverapp.backend.dto.GPSCoords;
import com.moses.notifications.Notification;
import com.moses.position.Position;
import com.moses.RabbitMqConnector;
import com.moses.marshalling.NotificationMarshaller;
import com.moses.marshalling.PositionMarshaller;
import com.rabbitmq.client.BuiltinExchangeType;

import java.io.IOException;
import java.util.concurrent.TimeoutException;
import java.util.function.Consumer;

public class  DriverAppConnector {
    private final static String exchangeName = "moses_exchange";
    private final static BuiltinExchangeType exchangeType = BuiltinExchangeType.DIRECT;

    private final static String localizationUpdateRoutingKey = "localization_update";

    private final String notificationQueue;

    private final static String host = "localhost";
    private final static String username = "moses";
    private final static String password = "split";

    private final RabbitMqConnector connector;

    public DriverAppConnector() throws IOException, TimeoutException {
        connector = new RabbitMqConnector(host, username, password);
        connector.setupExchange(exchangeName, exchangeType);
        notificationQueue = connector.setupQueue();
    }

    public void listenForNotifications(Consumer<Notification> userCallback) throws IOException {
        connector.listenForMessages(notificationQueue, (rawNotification, routingKey) ->  userCallback.accept(NotificationMarshaller.unmarshall(rawNotification, routingKey)));
    }

    public void sendLocalization(GPSCoords coords) throws IOException {
        byte[] messageBody = PositionMarshaller.marshallLocalizationUpdate(coords.latitude, coords.longitude);
        connector.sendMessage(exchangeName,
                localizationUpdateRoutingKey,
                messageBody);
    }

    public Position updateLocalization(GPSCoords coords) throws IOException, InterruptedException {
        byte[] requestBody = PositionMarshaller.marshallLocalizationUpdate(coords.latitude, coords.longitude);
        byte[] response = connector.performRPC(exchangeName,
                localizationUpdateRoutingKey,
                requestBody);
        return PositionMarshaller.unmarshallPosition(response);
    }

    public void bindWithKey(String routingKey) throws IOException {
        connector.bindQueue(notificationQueue, exchangeName, routingKey);
    }

    public void unbindKey(String routingKey) throws IOException {
        connector.unbindQueue(notificationQueue, exchangeName, routingKey);
    }

    public void shutdown() throws IOException {
        connector.closeConnection();
    }
}
