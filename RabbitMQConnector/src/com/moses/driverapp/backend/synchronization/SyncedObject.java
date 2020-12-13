package com.moses.driverapp.backend.synchronization;

import java.util.concurrent.locks.ReentrantLock;
import java.util.function.Consumer;
import java.util.function.Function;

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

    public void update(Function<T, T> updateOperation) {
        lock.lock();
        actualObject = updateOperation.apply(actualObject);
        lock.unlock();
    }

    public void update(Consumer<T> updateOperation) {
        lock.lock();
        updateOperation.accept(actualObject);
        lock.unlock();
    }

    public <R> R applyAndGet(Function<T, R> operation) {
        lock.lock();
        R result = operation.apply(actualObject);
        lock.unlock();
        return result;
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
