package com.moses.driverapp.backend.threads;

import com.moses.driverapp.backend.synchronization.ProcessedRides;
import com.moses.driverapp.backend.synchronization.SyncedObject;

public class ProcessedRidesCleaning extends Thread {
    private SyncedObject<Boolean> isInterrupted;
    private final ProcessedRides syncedProcessedRides;

    private final int interval = 1000;

    public ProcessedRidesCleaning(ProcessedRides syncedProcessedRides) {
        this.syncedProcessedRides = syncedProcessedRides;
    }

    @Override
    public void run() {
        isInterrupted = new SyncedObject<>(false);

        while(!isInterrupted.get()) {
            syncedProcessedRides.decreaseTTLs();
            try {
                Thread.sleep(interval);
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }
    }

    @Override
    public void interrupt() {
        isInterrupted.set(true);
        super.interrupt();
    }
}
