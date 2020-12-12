package com.moses.driverapp.simulation;

import com.moses.RabbitMqConnector;
import com.moses.marshalling.IdMarshaller;
import com.moses.driverapp.backend.dto.GPSCoords;
import com.moses.marshalling.PositionMarshaller;
import com.rabbitmq.client.BuiltinExchangeType;

import java.io.IOException;
import java.util.concurrent.TimeoutException;
import java.util.function.Consumer;

public class SimConnector {
    private final RabbitMqConnector connector;

    private String trackedVehicle;
    private final String listeningQueue;

    private final String simulationExchangeName = "moses_simulation_exchange";
    private final BuiltinExchangeType simulationExchangeType = BuiltinExchangeType.TOPIC;
    private final String trackingOnRoutingKey = "app.control.up";
    private final String trackingOffRoutingKey = "app.control.down";
    private final String vehicleTrackingRoutingKeyPrefix = "app.tracking";

    public SimConnector(RabbitMqConnector connector) throws IOException {
        this.connector = connector;
        connector.setupExchange(simulationExchangeName, simulationExchangeType);
        listeningQueue = connector.setupQueue();
    }

    public void stopTracking() throws IOException {
        if (trackedVehicle != null) {
            connector.sendMessage(simulationExchangeName, trackingOffRoutingKey, IdMarshaller.marshallId(trackedVehicle));
            connector.unbindQueue(listeningQueue, simulationExchangeName, vehicleTrackingRoutingKey());
            trackedVehicle = null;
        }
    }

    public void trackVehicle(String vehicleId) throws IOException {
        if (trackedVehicle != null) {
            stopTracking();
        }

        connector.sendMessage(simulationExchangeName, trackingOnRoutingKey, IdMarshaller.marshallId(vehicleId));
        trackedVehicle = vehicleId;
        connector.bindQueue(listeningQueue, simulationExchangeName, vehicleTrackingRoutingKey());
    }

    public void listenForCoords(Consumer<GPSCoords> callback) throws IOException {
        connector.listenForMessages(listeningQueue, (message, routingKey) -> callback.accept(PositionMarshaller.unmarshallGPSCoords(message)));
    }

    public void stopListening() throws IOException, TimeoutException {
        connector.closeChannel();
        connector.closeConnection();
    }

    private String vehicleTrackingRoutingKey() {
        return vehicleTrackingRoutingKeyPrefix + "." + trackedVehicle;
    }
}
