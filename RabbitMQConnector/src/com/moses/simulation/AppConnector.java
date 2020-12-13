package com.moses.simulation;

import com.moses.RabbitMqConnector;
import com.moses.driverapp.backend.synchronization.SyncedObject;
import com.moses.marshalling.IdMarshaller;
import com.moses.marshalling.PositionMarshaller;
import com.rabbitmq.client.BuiltinExchangeType;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.TimeoutException;

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

    public AppConnector() throws IOException, TimeoutException {
        this.connector = new RabbitMqConnector("localhost", "moses", "split");
        trackedVehicles = new SyncedObject<>(new ArrayList<>());

        connector.ensureConnected();
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

    public List<String> getTrackedVehicles() {
        return new ArrayList<>(trackedVehicles.get());
    }

    public void stop() throws IOException, TimeoutException {
        connector.closeChannel();
        connector.closeConnection();
    }

    private void addVehicleToTracked(String vehicleId) {
        trackedVehicles.apply(vehicleList -> {vehicleList.add(vehicleId);});
    }

    private void removeVehicleFromTracked(String vehicleId) {
        trackedVehicles.apply(vehicleList -> {vehicleList.remove(vehicleId);});
    }

    private String getTrackingRoutingKey(String vehicleId) {
        return trackingRoutingKeyPrefix + "." + vehicleId;
    }
}
