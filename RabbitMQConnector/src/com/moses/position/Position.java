package com.moses.position;

import com.moses.notifications.Notification;

public interface Position {
    String getRoutingKey();
    boolean doesNotificationApply(Notification notification);
}
