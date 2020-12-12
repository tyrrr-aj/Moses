package com.moses.driverapp.backend.threads;

import com.moses.driverapp.backend.DriverAppConnector;
import com.moses.driverapp.backend.interfaces.Displayer;
import com.moses.driverapp.backend.synchronization.ProcessedRides;
import com.moses.driverapp.backend.synchronization.SyncedObject;
import com.moses.notifications.Notification;
import com.moses.position.Position;

public class NotificationsProcessor {
    private final ProcessedRides alreadyProcessedRides;
    private final DriverAppConnector connector;
    private final SyncedObject<Position> syncedPosition;
    private final Displayer displayer;

    public NotificationsProcessor(ProcessedRides alreadyProcessedRides, DriverAppConnector connector, SyncedObject<Position> syncedPosition, Displayer displayer) {
        this.alreadyProcessedRides = alreadyProcessedRides;
        this.connector = connector;
        this.syncedPosition = syncedPosition;
        this.displayer = displayer;
    }

    public void process(Notification notification) {
        if (syncedPosition.get().doesNotificationApply(notification)) {
            if (alreadyProcessedRides.isPresent(notification.rideId)) {
                alreadyProcessedRides.resetTTL(notification.rideId);
            }
            else {
                alreadyProcessedRides.addRide(notification.rideId);
                displayer.displayNotification(notification);
            }
        }
    }
}
