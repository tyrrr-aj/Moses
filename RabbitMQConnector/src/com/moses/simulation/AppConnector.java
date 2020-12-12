package com.moses.simulation;

import com.moses.RabbitMqConnector;
import com.moses.driverapp.backend.synchronization.SyncedObject;
import com.moses.marshalling.IdMarshaller;
import com.moses.marshalling.PositionMarshaller;
import com.rabbitmq.client.BuiltinExchangeType;
import com.rabbitmq.client.Channel;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeoutException;
import java.util.function.Consumer;

public class AppConnector {
    private RabbitMqConnector connector;

    private SyncedObject<List<String>> trackedVehicles;
    private ExecutorService executor;

    private final String trackingOnQueue;
    private final String trackingOffQueue;

    private final String simulationExchangeName = "moses_simulation_exchange";
    private final BuiltinExchangeType simulationExchangeType = BuiltinExchangeType.TOPIC;
    private final String trackingOnRoutingKey = "app.control.up";
    private final String trackingOffRoutingKey = "app.control.down";
    private final String trackingRoutingKeyPrefix = "app.tracking";

    public AppConnector(RabbitMqConnector connector) throws IOException {
        this.connector = connector;
        trackedVehicles = new SyncedObject<>(new ArrayList<>());

        connector.setupExchange(simulationExchangeName, simulationExchangeType);
        trackingOnQueue = connector.setupAndBindQueue(simulationExchangeName, trackingOnRoutingKey);
        trackingOffQueue = connector.setupAndBindQueue(simulationExchangeName, trackingOffRoutingKey);
    }

    public void startTracking() throws IOException {
        connector.listenForMessages(trackingOnQueue, (rawMessage, routingKey) -> addVehicleToTracked(IdMarshaller.unmarshallId(rawMessage)));
        connector.listenForMessages(trackingOffQueue, (rawMessage, routingKey) -> removeVehicleFromTracked(IdMarshaller.unmarshallId(rawMessage)));
    }

    public void updateVehiclePosition(String vehicleId, double lon, double lat) throws IOException {
        connector.sendMessage(simulationExchangeName, getTrackingRoutingKey(vehicleId), PositionMarshaller.marshallLocalizationUpdate(lat, lon));
    }

    public void stop() throws IOException, TimeoutException {
        connector.closeChannel();
        connector.closeConnection();
    }

    private void addVehicleToTracked(String vehicleId) {
        List<String> vehicleList = trackedVehicles.startUpdate();
        vehicleList.add(vehicleId);
        trackedVehicles.endUpdate();
    }

    private void removeVehicleFromTracked(String vehicleId) {
        List<String> vehicleList = trackedVehicles.startUpdate();
        vehicleList.remove(vehicleId);
        trackedVehicles.endUpdate();
    }

    private String getTrackingRoutingKey(String vehicleId) {
        return trackingRoutingKeyPrefix + "." + vehicleId;
    }
}
