package com.moses.position;

import com.moses.notifications.Notification;
import com.moses.notifications.RoadBounds;

public class PositionOnRoad implements Position {
    public final String roadId;
    public final double partOfRoad;
    public Direction direction;

    public PositionOnRoad(String roadId, double partOfRoad) {
        this.roadId = roadId;
        this.partOfRoad = partOfRoad;
        this.direction = Direction.UNKNOWN;
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

//            if (partOfRoad <= roadBounds.beginAt || partOfRoad >= roadBounds.endAt) {
//                System.out.println("Rejected based on part of road");
//            }
//            if (direction == Direction.UNKNOWN) {
//                System.out.println("Rejected based on direction unknown");
//            }
//            else if (!direction.equals(roadBounds.direction)) {
//                System.out.println("Rejected based on different directions");
//            }

            return partOfRoad > roadBounds.beginAt && partOfRoad < roadBounds.endAt
                    && !direction.equals(Direction.UNKNOWN) && direction.equals(roadBounds.direction);
        }
        else {
            return false;
        }
    }

    public void setHistoricalInfo(Position previousPosition) {
        this.direction = calculateDirection(previousPosition);
    }

    private Direction calculateDirection(Position previousPosition) {
        if (previousPosition instanceof PositionOnRoad) {
            PositionOnRoad typedPreviousPosition = (PositionOnRoad) previousPosition;
            if (roadId.equals(typedPreviousPosition.roadId)) {
                if (partOfRoad > typedPreviousPosition.partOfRoad) {
                    return Direction.FORWARD;
                }
                else if (partOfRoad < typedPreviousPosition.partOfRoad) {
                    return Direction.BACKWARD;
                }
            }
        }

        return Direction.UNKNOWN;
    }
}
