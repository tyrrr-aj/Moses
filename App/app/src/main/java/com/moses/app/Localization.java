package com.moses.app;

public class Localization {
    public String RoadId;
    public float PartOfRoad;
    public Direction direction;

    public Localization() {
        RoadId = "unknown";
        PartOfRoad = (float) 0.0;
        direction = Direction.FORWARD;
    }

    public enum Direction {FORWARD, BACKWARD}
}
