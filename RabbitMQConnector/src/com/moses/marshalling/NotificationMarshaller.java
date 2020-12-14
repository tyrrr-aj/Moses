package com.moses.marshalling;

import com.moses.notifications.JunctionBounds;
import com.moses.notifications.Notification;
import com.moses.notifications.NotificationType;
import com.moses.notifications.RoadBounds;
import com.moses.position.Direction;

import java.util.Arrays;

public class NotificationMarshaller {
    public static Notification unmarshall(byte[] rawNotification, String routingKey) {
        switch (getRoadNetworkElementType(rawNotification)) {
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
        notification.type = getNotificationType(rawNotification);

        RoadBounds roadBounds = new RoadBounds();
        roadBounds.beginAt = ((float) (int) rawNotification[2]) / 100;
        roadBounds.endAt = ((float) (int) rawNotification[3]) / 100;
        roadBounds.direction = (int) rawNotification[4] == 0 ? Direction.BACKWARD : Direction.FORWARD;

        notification.geographicalBounds = roadBounds;
        notification.geographicalBounds.roadNetworkElementId = routingKey;

        notification.rideId = new String(Arrays.copyOfRange(rawNotification, 5, rawNotification.length));

        return notification;
    }

    private static Notification unmarshallJunctionNotification(byte[] rawNotification, String routingKey) {
        Notification notification = new Notification();
        notification.type = getNotificationType(rawNotification);

        notification.geographicalBounds = new JunctionBounds();
        notification.geographicalBounds.roadNetworkElementId = routingKey;

        notification.rideId = new String(Arrays.copyOfRange(rawNotification, 2, rawNotification.length));

        return notification;
    }

    private static RoadNetworkElementType getRoadNetworkElementType(byte[] rawNotification) {
        return ((int)(rawNotification[0]) == 0) ? RoadNetworkElementType.ROAD : RoadNetworkElementType.JUNCTION;
    }

    private static NotificationType getNotificationType(byte[] rawNotification) {
        switch ((int) rawNotification[1]) {
            case 0:
                return NotificationType.MAKE_WAY_ON_ROAD;

            case 1:
                return NotificationType.MAKE_WAY_ON_JUNCTION;

            case 2:
                return NotificationType.NO_ACTION_REQUIRED;

            default:
                throw new IllegalArgumentException(String.format("Notification had type code outside range: %d", (int) rawNotification[1]));
        }
    }

    private enum RoadNetworkElementType {
        ROAD, JUNCTION
    }
}
