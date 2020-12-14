package com.moses.driverapp.backend.threads;

import com.moses.driverapp.backend.DriverAppConnector;
import com.moses.driverapp.backend.interfaces.Displayer;
import com.moses.driverapp.backend.synchronization.ProcessedRides;
import com.moses.driverapp.backend.synchronization.SyncedObject;
import com.moses.notifications.Notification;
import com.moses.position.Position;

import java.util.function.Function;

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
        if (syncedPosition.applyAndGet((position) -> position.doesNotificationApply(notification))) {
            System.out.println(String.format("Notification for %s ACCEPTED (own routing key is %s)",
                    notification.geographicalBounds.roadNetworkElementId, syncedPosition.applyAndGet(Position::getRoutingKey)));
            if (alreadyProcessedRides.isPresent(notification.rideId)) {
                alreadyProcessedRides.resetTTL(notification.rideId);

                System.out.println("...but DISMISSED as already processed");
            }
            else {
                alreadyProcessedRides.addRide(notification.rideId);
                displayer.displayNotification(notification);
            }
        }
        else {
            System.out.println(String.format("Notification for %s REJECTED (own routing key is %s)",
                    notification.geographicalBounds.roadNetworkElementId, syncedPosition.applyAndGet(Position::getRoutingKey)));
        }
    }
}
