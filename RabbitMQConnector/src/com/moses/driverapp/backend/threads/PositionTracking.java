package com.moses.driverapp.backend.threads;

import com.moses.driverapp.backend.DriverAppConnector;
import com.moses.driverapp.backend.dto.GPSCoords;
import com.moses.driverapp.backend.interfaces.GPSAccessor;
import com.moses.driverapp.backend.synchronization.SyncedObject;
import com.moses.position.Position;
import com.moses.position.UnknownPosition;

import java.io.IOException;
import java.util.concurrent.TimeoutException;

public class PositionTracking extends Thread {
    private SyncedObject<Boolean> isInterrupted;

    private final DriverAppConnector connector;
    private final SyncedObject<Position> syncedPosition;
    private final GPSAccessor gpsAccessor;

    private Position lastKnownPosition = new UnknownPosition();

    private final int interval = 1000;

    public PositionTracking(DriverAppConnector connector, SyncedObject<Position> syncedPosition, GPSAccessor gpsAccessor) {
        this.connector = connector;
        this.syncedPosition = syncedPosition;
        this.gpsAccessor = gpsAccessor;
    }

    @Override
    public void run() {
        isInterrupted = new SyncedObject<>(false);
        try {
            connector.ensureConnected();
        } catch (IOException | TimeoutException e) {
            e.printStackTrace();
            return;
        }

        while(!isInterrupted.get()) {
            GPSCoords coords = gpsAccessor.getCurrentCoords();
            if (coords != null) {
                try {
                    Position currentPosition = connector.updateLocalization(coords);

                    if (currentPosition != null) {
                        System.out.println("Road: " + currentPosition.getRoutingKey());

                        updatePosition(currentPosition);

                        if (routingKeyHasChanged(currentPosition)) {
                            updateBindings(currentPosition, lastKnownPosition);
                        }

                        lastKnownPosition = currentPosition;
                    }
                } catch (IOException | InterruptedException e) {
                    e.printStackTrace();
                    return;
                }
            }

            try {
                Thread.sleep(interval);
            } catch (InterruptedException e) {
                return;
            }
        }
    }

    @Override
    public void interrupt() {
        isInterrupted.set(true);
        super.interrupt();
    }

    private void updatePosition(Position newPosition) {
        newPosition.setHistoricalInfo(lastKnownPosition);
        syncedPosition.set(newPosition);
    }

    private void updateBindings(Position currentPosition, Position oldPosition) throws IOException {
        connector.bindWithKey(currentPosition.getRoutingKey());
        if (!oldPosition.getRoutingKey().equals("unknown")) {
            connector.unbindKey(oldPosition.getRoutingKey());
        }
    }

    private boolean routingKeyHasChanged(Position currentPosition) {
        return !currentPosition.getRoutingKey().equals(lastKnownPosition.getRoutingKey());
    }
}
