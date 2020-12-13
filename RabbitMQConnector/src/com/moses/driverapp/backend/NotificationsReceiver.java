package com.moses.driverapp.backend;

import com.moses.RabbitMqConnector;
import com.moses.driverapp.backend.interfaces.Displayer;
import com.moses.driverapp.backend.interfaces.GPSAccessor;
import com.moses.driverapp.backend.synchronization.ProcessedRides;
import com.moses.driverapp.backend.synchronization.SyncedObject;
import com.moses.driverapp.backend.threads.NotificationsProcessor;
import com.moses.driverapp.backend.threads.PositionTracking;
import com.moses.driverapp.backend.threads.ProcessedRidesCleaning;
import com.moses.position.Position;
import com.moses.position.UnknownPosition;

import java.io.IOException;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeoutException;

public class NotificationsReceiver {
    private final GPSAccessor gpsAccessor;
    private final Displayer displayer;
    DriverAppConnector connector;

    ExecutorService executor;

    public NotificationsReceiver(GPSAccessor gpsAccessor, Displayer displayer, RabbitMqConnector rabbitMqConnector) {
        this.gpsAccessor = gpsAccessor;
        this.displayer = displayer;
        connector = new DriverAppConnector(rabbitMqConnector);
    }

    public NotificationsReceiver(GPSAccessor gpsAccessor, Displayer displayer) {
        this.gpsAccessor = gpsAccessor;
        this.displayer = displayer;
        connector = new DriverAppConnector();
    }

    public void receiveNotifications() throws IOException, TimeoutException {
        ProcessedRides alreadyProcessedRides = new ProcessedRides();
        SyncedObject<Position> syncedPosition = new SyncedObject<>(new UnknownPosition());

        NotificationsProcessor processor = new NotificationsProcessor(alreadyProcessedRides, connector, syncedPosition, displayer);
        PositionTracking positionTracking = new PositionTracking(connector, syncedPosition, gpsAccessor);
        ProcessedRidesCleaning processedRidesCleaning = new ProcessedRidesCleaning(alreadyProcessedRides);

        executor = Executors.newFixedThreadPool(3);

        executor.execute(positionTracking);
        executor.execute(processedRidesCleaning);
        executor.execute(() -> {
            try {
                connector.ensureConnected();
                connector.listenForNotifications(processor::process);
            } catch (IOException | TimeoutException e) {
                e.printStackTrace();
            }
        });
    }

    public void shutdown() {
        executor.shutdownNow();
        CompletableFuture.runAsync(() -> {
            try {
                connector.shutdown();
            } catch (IOException e) {
                e.printStackTrace();
            }
        });
    }
}
