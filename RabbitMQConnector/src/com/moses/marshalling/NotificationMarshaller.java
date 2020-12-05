package com.moses.marshalling;

import com.moses.notifications.GeographicalBounds;
import com.moses.notifications.JunctionBounds;
import com.moses.notifications.Notification;
import com.moses.notifications.RoadBounds;

import java.util.Arrays;

public class NotificationMarshaller {
    public static Notification unmarshall(byte[] rawNotification, String routingKey) {


        switch (getType(rawNotification)) {
            case ROAD:
                return unmarshallRoadNotification(rawNotification, routingKey);

            case JUNCTION:
                return unmarshallJunctionNotification(rawNotification, routingKey);

            default:
                return null;
        }
    }

    private static Notification unmarshallRoadNotification(byte[] rawNotification, String routingKey) {
        Notification notification = new Notification();
        int idEndIndex = new String(rawNotification).indexOf(";");

        RoadBounds roadBounds = new RoadBounds();
        roadBounds.beginAt = ((float) (int) rawNotification[1]) / 100;
        roadBounds.endAt = ((float) (int) rawNotification[2]) / 100;
        roadBounds.direction = Notification.Direction.values()[(int) rawNotification[3]];

        notification.geographicalBounds = roadBounds;
        notification.geographicalBounds.roadNetworkElementId = routingKey;

        notification.rideId = new String(Arrays.copyOfRange(rawNotification, 4, idEndIndex));
        notification.text = new String(Arrays.copyOfRange(rawNotification, idEndIndex + 1, rawNotification.length));

        return notification;
    }

    private static Notification unmarshallJunctionNotification(byte[] rawNotification, String routingKey) {
        Notification notification = new Notification();
        int idEndIndex = new String(rawNotification).indexOf(";");

        notification.geographicalBounds = new JunctionBounds();
        notification.geographicalBounds.roadNetworkElementId = routingKey;

        notification.rideId = new String(Arrays.copyOfRange(rawNotification, 1, idEndIndex));
        notification.text = new String(Arrays.copyOfRange(rawNotification, idEndIndex + 1, rawNotification.length));

        return notification;
    }

    private static NotificationType getType(byte[] rawNotification) {
        return ((int)(rawNotification[0]) == 0) ? NotificationType.ROAD : NotificationType.JUNCTION;
    }

    private enum NotificationType {
        ROAD, JUNCTION
    }
}
