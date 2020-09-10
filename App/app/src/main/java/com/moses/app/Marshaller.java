package com.moses.app;

import java.util.Arrays;

public class Marshaller {
    public Notification unmarshall(byte[] rawNotification) {
        Notification notification = new Notification();

        notification.beginAt = ((float) (int) rawNotification[0]) / 100;
        notification.endAt = ((float) (int) rawNotification[1]) / 100;
        notification.direction = Notification.Direction.values()[(int) rawNotification[2]];
        notification.text = new String(Arrays.copyOfRange(rawNotification, 3, rawNotification.length));

        return notification;
    }

    public byte[] marshallLocalizationUpdate(double latitude, double longitude, String queueName) {
        return String.format("%d%d%s", gpsCoordToInt(latitude), gpsCoordToInt(longitude), queueName).getBytes();
    }

    private int gpsCoordToInt(double coord) {
        return (int) (coord * 100000);
    }
}
