package com.moses.driverapp.backend.synchronization;

import java.util.concurrent.locks.ReentrantLock;

public class SyncedObject<T> {
    private T actualObject;
    private ReentrantLock lock;

    public SyncedObject(T initialValue) {
        lock = new ReentrantLock();
        actualObject = initialValue;
    }

    public void set(T newValue) {
        lock.lock();
        actualObject = newValue;
        lock.unlock();
    }

    public T get() {
        lock.lock();
        T readValue = actualObject;
        lock.unlock();
        return readValue;
    }

    public T startUpdate() {
        lock.lock();
        return actualObject;
    }

    public void setValueDuringUpdate(T newValue) {
        actualObject = newValue;
    }

    public void endUpdate() {
        lock.unlock();
    }
}
