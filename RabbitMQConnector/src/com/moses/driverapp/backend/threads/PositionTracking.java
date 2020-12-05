package com.moses.driverapp.backend.threads;

import com.moses.driverapp.backend.DriverAppConnector;
import com.moses.driverapp.backend.dto.GPSCoords;
import com.moses.driverapp.backend.interfaces.GPSAccessor;
import com.moses.driverapp.backend.synchronization.SyncedObject;
import com.moses.position.Position;
import com.moses.position.UnknownPosition;

import java.io.IOException;

public class PositionTracking extends Thread {
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
        while(true) {
            GPSCoords coords = gpsAccessor.getCurrentCoords();
            try {
                Position currentPosition = connector.updateLocalization(coords);

                if (currentPosition != null) {
                    updatePosition(currentPosition);

                    if (routingKeyHasChanged(currentPosition)) {
                        updateBindings(currentPosition, lastKnownPosition);
                    }

                    lastKnownPosition = currentPosition;
                }
            } catch (IOException | InterruptedException e) {
                e.printStackTrace();
            }

            try {
                Thread.sleep(interval);
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }
    }

    private void updatePosition(Position newPosition) {
        syncedPosition.set(newPosition);
    }

    private void updateBindings(Position currentPosition, Position oldPosition) throws IOException {
        connector.bindWithKey(currentPosition.getRoutingKey());
        connector.unbindKey(oldPosition.getRoutingKey());
    }

    private boolean routingKeyHasChanged(Position currentPosition) {
        return !currentPosition.getRoutingKey().equals(lastKnownPosition.getRoutingKey());
    }
}
