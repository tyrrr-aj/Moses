package com.moses.driverapp.backend.dto;

public class GPSCoords {
    public final double longitude;
    public final double latitude;

    public GPSCoords(double longitude, double latitude) {
        this.longitude = longitude;
        this.latitude = latitude;
    }

    @Override
    public String toString() {
        return String.format("(%f,%f)", latitude, longitude);
    }
}
