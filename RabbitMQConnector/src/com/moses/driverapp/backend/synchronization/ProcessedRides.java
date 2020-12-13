package com.moses.driverapp.backend.synchronization;

import java.util.HashMap;
import java.util.Map;
import java.util.stream.Collectors;

public class ProcessedRides {
    private final SyncedObject<Map<String, Integer>> processedRides;

    private final int initialTTL = 10;

    public ProcessedRides() {
        processedRides = new SyncedObject<>(new HashMap<>());
    }

    public void addRide(String rideId) {
        processedRides.apply(rides -> {rides.put(rideId, initialTTL);});
    }

    public void decreaseTTLs() {
        processedRides.update(rides -> {return
            rides.entrySet()
                .stream()
                .filter(ride -> ride.getValue() > 1)
                .collect(Collectors.toMap(Map.Entry::getKey, ride -> ride.getValue() - 1));}
                );
    }

    public void resetTTL(String rideId) {
        processedRides.apply(rides -> {rides.put(rideId, initialTTL);});
    }

    public boolean isPresent(String rideId) {
        return processedRides.applyAndGet(rides -> rides.containsKey(rideId));
    }
}
