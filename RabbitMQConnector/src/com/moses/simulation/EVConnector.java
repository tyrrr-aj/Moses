package com.moses.simulation;

import com.moses.RabbitMqConnector;
import com.rabbitmq.client.BuiltinExchangeType;

import java.io.IOException;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.TimeoutException;

public class EVConnector {
    private final static String host = "172.18.32.1";
    private final static String username = "moses";
    private final static String password = "split";

    private final static String exchangeName = "moses_simulation_exchange";
    private final static BuiltinExchangeType exchangeType = BuiltinExchangeType.TOPIC;

    private final static String dispatchRootingKeyRoot = "dispatch";
    private final static String trackingRootingKeyRoot = "tracking";

    private final String dispatchRootingKey;
    private final String trackingRootingKey;

    private final String vehicleId;

    private final RabbitMqConnector connector;
    private final Marshaller marshaller;

    public EVConnector(String evType, String vehicleId) throws IOException, TimeoutException {
        connector = new RabbitMqConnector(host, username, password);
        connector.setupExchange(exchangeName, exchangeType);

        dispatchRootingKey = dispatchRootingKeyRoot + "." + evType;
        trackingRootingKey = trackingRootingKeyRoot + "." + evType;
        this.vehicleId = vehicleId;

        marshaller = new Marshaller();
    }

    public void sendDispatchMessageAsync(double longitude, double latitude) {
        CompletableFuture.runAsync(() -> {
            byte[] messageBody = marshaller.marshall(longitude, latitude, vehicleId);
            try {
                connector.sendMessage(exchangeName, dispatchRootingKey, messageBody);
            } catch (IOException e) {
                System.out.println("Error while sending dispatch message from " + vehicleId);
            }
        });
    }

    public void sendTrackingMessageAsync(double longitude, double latitude) {
        CompletableFuture.runAsync(() -> {
            byte[] messageBody = marshaller.marshall(longitude, latitude, vehicleId);
            try {
                connector.sendMessage(exchangeName, trackingRootingKey, messageBody);
            } catch (IOException e) {
                System.out.println("Error while sending dispatch message from " + vehicleId);
            }
        });
    }

    public void sendEndRideMessageAsync() {
        CompletableFuture.runAsync(() -> {
            byte[] messageBody = marshaller.marshallEndRide(vehicleId);
            try {
                connector.sendMessage(exchangeName, dispatchRootingKey, messageBody);
            } catch (IOException e) {
                System.out.println("Error while sending end_ride message from " + vehicleId);
            }
        });
    }
}
