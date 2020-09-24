package com.moses.simulation;

public class Marshaller {
    public byte[] marshall(double longitude, double latitude, String vehicleId) {
        return String.format("%d%d%s", gpsCoordToInt(latitude), gpsCoordToInt(longitude), vehicleId).getBytes();
    }

    private int gpsCoordToInt(double coord) {
        return (int) (coord * 100000);
    }
}
