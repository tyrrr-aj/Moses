package com.moses;

public class Position {
    public final String roadId;
    public final double partOfRoad;

    public Position(String roadId, double partOfRoad) {
        this.roadId = roadId;
        this.partOfRoad = partOfRoad;
    }

    @Override
    public String toString() {
        return "{" + roadId + ", " +  partOfRoad + '}';
    }
}
