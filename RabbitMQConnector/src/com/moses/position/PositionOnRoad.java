package com.moses.position;

import com.moses.notifications.Notification;
import com.moses.notifications.RoadBounds;

public class PositionOnRoad implements Position {
    public final String roadId;
    public final double partOfRoad;

    public PositionOnRoad(String roadId, double partOfRoad) {
        this.roadId = roadId;
        this.partOfRoad = partOfRoad;
    }

    @Override
    public String toString() {
        return "{" + roadId + ":" +  partOfRoad + '}';
    }

    @Override
    public String getRoutingKey() {
        return roadId;
    }

    @Override
    public boolean doesNotificationApply(Notification notification) {
        if (notification.geographicalBounds.roadNetworkElementId.equals(roadId)) {
            RoadBounds roadBounds = (RoadBounds) notification.geographicalBounds;
            return (roadBounds.direction.equals(Notification.Direction.FORWARD) && partOfRoad > roadBounds.beginAt && partOfRoad < roadBounds.endAt)
                    || (roadBounds.direction.equals(Notification.Direction.BACKWARD) && partOfRoad < roadBounds.beginAt && partOfRoad > roadBounds.endAt);
        }
        else {
            return false;
        }
    }
}
