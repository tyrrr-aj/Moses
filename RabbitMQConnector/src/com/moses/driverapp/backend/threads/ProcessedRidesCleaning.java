package com.moses.driverapp.backend.threads;

import com.moses.driverapp.backend.synchronization.ProcessedRides;

public class ProcessedRidesCleaning extends Thread {
    private final ProcessedRides syncedProcessedRides;

    private final int interval = 1000;

    public ProcessedRidesCleaning(ProcessedRides syncedProcessedRides) {
        this.syncedProcessedRides = syncedProcessedRides;
    }

    @Override
    public void run() {
        while(true) {
            syncedProcessedRides.decreaseTTLs();
            try {
                Thread.sleep(interval);
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }
    }
}
