package com.moses.app;

import androidx.annotation.NonNull;

public class Notification {
    public float beginAt;
    public float endAt;
    public Direction direction;
    public String text;

    public enum Direction {FORWARD, BACKWARD}

    @NonNull
    @Override
    public String toString() {
        return String.format("start=%f, end=%f, direction=%s, text=\"%s\"", beginAt, endAt, direction, text);
    }
}
