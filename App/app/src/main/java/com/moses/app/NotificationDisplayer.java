package com.moses.app;

import android.content.Intent;

import com.moses.driverapp.backend.interfaces.Displayer;
import com.moses.notifications.Notification;

import static com.moses.app.MainActivity.MESSAGE_BODY;

public class NotificationDisplayer implements Displayer {
    private MainActivity mainActivity;

    public NotificationDisplayer(MainActivity mainActivity) {
        this.mainActivity = mainActivity;
    }

    @Override
    public void displayNotification(Notification notification) {
        Intent intent = new Intent(mainActivity, ShowNotification.class);
        intent.putExtra(MESSAGE_BODY, notification.rideId + ": " + notification.text);
        mainActivity.startActivity(intent);
    }
}
