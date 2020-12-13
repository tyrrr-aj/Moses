package com.moses.driverapp.simulation;

import com.moses.driverapp.backend.dto.GPSCoords;
import com.moses.driverapp.backend.interfaces.GPSAccessor;
import com.moses.driverapp.backend.synchronization.SyncedObject;

import java.io.IOException;

public class SimulatedGPSAccessor implements GPSAccessor {
    private SimConnector simConnector;
    private SyncedObject<GPSCoords> currentCoords;

    public SimulatedGPSAccessor(SimConnector simConnector) throws IOException {
        this.simConnector = simConnector;
        currentCoords = new SyncedObject<>(null);
    }

    public void init(){
        simConnector.ensureConnected();
        simConnector.listenForCoords(this::updateCoords);
    }

    @Override
    public GPSCoords getCurrentCoords() {
        return currentCoords.get();
    }

    private void updateCoords(GPSCoords coords) {
        currentCoords.set(coords);
    }
}
