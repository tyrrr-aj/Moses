package com.moses.notifications;

public class Notification {
    public String rideId;
    public GeographicalBounds geographicalBounds;
    public String text;

    public enum Direction {FORWARD, BACKWARD}
}
