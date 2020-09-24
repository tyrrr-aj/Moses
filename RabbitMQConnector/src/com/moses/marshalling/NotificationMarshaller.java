package com.moses.marshalling;

import com.moses.driverapp.Notification;

import java.util.Arrays;

public class NotificationMarshaller {
    public static Notification unmarshall(byte[] rawNotification) {
        Notification notification = new Notification();

        notification.beginAt = ((float) (int) rawNotification[0]) / 100;
        notification.endAt = ((float) (int) rawNotification[1]) / 100;
        notification.direction = Notification.Direction.values()[(int) rawNotification[2]];
        notification.text = new String(Arrays.copyOfRange(rawNotification, 3, rawNotification.length));

        return notification;
    }
}
