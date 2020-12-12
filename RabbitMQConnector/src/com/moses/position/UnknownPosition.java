package com.moses.position;

import com.moses.notifications.Notification;

public class UnknownPosition implements Position {
    @Override
    public String getRoutingKey() {
        return "unknown";
    }

    @Override
    public boolean doesNotificationApply(Notification notification) {
        return false;
    }
}
