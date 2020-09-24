package com.moses.driverapp;

public class Notification {
    public float beginAt;
    public float endAt;
    public Direction direction;
    public String text;

    public enum Direction {FORWARD, BACKWARD}
}
