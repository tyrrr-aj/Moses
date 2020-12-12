package com.moses.driverapp.backend.interfaces;

import com.moses.driverapp.backend.dto.GPSCoords;

public interface GPSAccessor {
    GPSCoords getCurrentCoords();
}
