package com.moses.driverapp.simulation;

import com.moses.RabbitMqConnector;
import com.moses.marshalling.IdMarshaller;
import com.moses.driverapp.backend.dto.GPSCoords;
import com.moses.marshalling.PositionMarshaller;
import com.rabbitmq.client.BuiltinExchangeType;

import java.io.IOException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeoutException;
import java.util.function.Consumer;

public class SimConnector {
    private final RabbitMqConnector connector;

    private String trackedVehicle;
    private String listeningQueue = null;

    private final String simulationExchangeName = "moses_simulation_exchange";
    private final BuiltinExchangeType simulationExchangeType = BuiltinExchangeType.TOPIC;
    private final String trackingOnRoutingKey = "app.control.up";
    private final String trackingOffRoutingKey = "app.control.down";
    private final String vehicleTrackingRoutingKeyPrefix = "app.tracking";

    private final ExecutorService executor;

    public SimConnector(RabbitMqConnector connector) throws IOException {
        this.connector = connector;
        executor = Executors.newSingleThreadExecutor();
    }

    public void ensureConnected() {
        executor.submit(() -> {
            if (listeningQueue == null) {
                try {
                    setupRabbitMqConnectorTask();
                } catch (IOException | TimeoutException e) {
                    e.printStackTrace();
                }
            }
        });
    }

    public void stopTracking() {
        executor.submit(this::stopTrackingTask);
    }

    public void trackVehicle(String vehicleId) {
        executor.submit(() -> {
            if (trackedVehicle != null) {
                stopTrackingTask();
            }

            startTrackingTask(vehicleId);
        });
    }

    void listenForCoords(Consumer<GPSCoords> callback) {
        executor.submit(() -> {
            try {
                connector.listenForMessages(listeningQueue, (message, routingKey) -> callback.accept(PositionMarshaller.unmarshallGPSCoords(message)));
            } catch (IOException e) {
                e.printStackTrace();
            }
        });
    }

    public void stopListening() {
        executor.submit(() -> {
            try {
                connector.closeChannel();
                connector.closeConnection();
            } catch (IOException | TimeoutException e) {
                e.printStackTrace();
            }
        });
        executor.shutdown();
    }

    private void stopTrackingTask() {
        if (trackedVehicle != null) {
            try {
                connector.sendMessage(simulationExchangeName, trackingOffRoutingKey, IdMarshaller.marshallId(trackedVehicle));
                connector.unbindQueue(listeningQueue, simulationExchangeName, vehicleTrackingRoutingKey());
                trackedVehicle = null;
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
    }

    private void startTrackingTask(String vehicleId) {
        try {
            connector.sendMessage(simulationExchangeName, trackingOnRoutingKey, IdMarshaller.marshallId(vehicleId));
            trackedVehicle = vehicleId;
            connector.bindQueue(listeningQueue, simulationExchangeName, vehicleTrackingRoutingKey());
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private void setupRabbitMqConnectorTask() throws IOException, TimeoutException {
        connector.ensureConnected();
        connector.setupExchange(simulationExchangeName, simulationExchangeType);
        listeningQueue = connector.setupQueue();
    }

    private String vehicleTrackingRoutingKey() {
        return vehicleTrackingRoutingKeyPrefix + "." + trackedVehicle;
    }
}
