package com.moses.app;

import android.location.Location;
import android.location.LocationListener;

import androidx.annotation.NonNull;

public class MyLocationListener implements LocationListener
{

    public double longitude;
    public double latitude;

    @Override
    public void onLocationChanged(@NonNull Location location) {
        longitude = location.getLongitude();
        latitude = location.getLatitude();
    }
}
