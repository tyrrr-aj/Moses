package com.moses.driverapp.backend.synchronization;

import java.util.HashMap;
import java.util.Map;
import java.util.stream.Collectors;

public class ProcessedRides {
    private final SyncedObject<Map<String, Integer>> processedRides;

    private final int initialTTL = 10;
    private final int interval = 1000;

    public ProcessedRides() {
        processedRides = new SyncedObject<>(new HashMap<>());
    }

    public void addRide(String rideId) {
        processedRides.get().put(rideId, initialTTL);
    }

    public void decreaseTTLs() {
        Map<String, Integer> oldRides = processedRides.startUpdate();
        processedRides.setValueDuringUpdate(oldRides
                .entrySet()
                .stream()
                .filter(ride -> ride.getValue() > 1)
                .collect(Collectors.toMap(Map.Entry::getKey, ride -> ride.getValue() - 1)));
        processedRides.endUpdate();
    }

    public void resetTTL(String rideId) {
        Map<String, Integer> rides = processedRides.startUpdate();
        rides.put(rideId, initialTTL);
        processedRides.endUpdate();
    }

    public boolean isPresent(String rideId) {
        return processedRides.get().containsKey(rideId);
    }
}
