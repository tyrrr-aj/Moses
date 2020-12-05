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
        processedRides.set(processedRides.get()
                .entrySet()
                .stream()
                .map(ride -> Map.entry(ride.getKey(), ride.getValue() - 1))
                .filter(ride -> ride.getValue() > 0)
                .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue)));
    }

    public void resetTTL(String rideId) {
        processedRides.get().put(rideId, initialTTL);
    }

    public boolean isPresent(String rideId) {
        return processedRides.get().containsKey(rideId);
    }
}
