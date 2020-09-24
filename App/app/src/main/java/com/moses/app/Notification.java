package com.moses.app;

import androidx.annotation.NonNull;

public class Notification {
    public float beginAt;
    public float endAt;
    public Localization.Direction direction;
    public String text;

    @NonNull
    @Override
    public String toString() {
        return String.format("start=%f, end=%f, direction=%s, text=\"%s\"", beginAt, endAt, direction, text);
    }
}
