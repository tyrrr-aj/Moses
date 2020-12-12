package com.moses.position;

import com.moses.notifications.Notification;

public class PositionOnJunction implements Position {
    public final String junctionId;

    public PositionOnJunction(String junctionId) {
        this.junctionId = junctionId;
    }

    @Override
    public String getRoutingKey() {
        return junctionId;
    }

    @Override
    public boolean doesNotificationApply(Notification notification) {
        return notification.geographicalBounds.roadNetworkElementId.equals(junctionId);
    }

    @Override
    public String toString() {
        return junctionId;
    }
}
