package com.moses.app;

import android.content.Intent;
import android.graphics.Color;
import android.util.Log;

import com.moses.driverapp.backend.interfaces.Displayer;
import com.moses.notifications.Notification;
import com.moses.notifications.NotificationType;

import static com.moses.app.MainActivity.MESSAGE_BODY;

public class NotificationDisplayer implements Displayer {
    private MainActivity mainActivity;

    public NotificationDisplayer(MainActivity mainActivity) {
        this.mainActivity = mainActivity;
    }

    @Override
    public void displayNotification(Notification notification) {
        Log.d("Notification displayer", "NOTIFICATION ARRIVED!");
        mainActivity.showMessage(title(notification.type), message(notification.type), color(notification.type));
    }

    private String title(NotificationType notificationType) {
        switch (notificationType) {
            case MAKE_WAY_ON_ROAD:
                return "Utwórz korytarz życia";

            case MAKE_WAY_ON_JUNCTION:
                return "Ustąp pierwszeństwa na skrzyżowaniu";

            case NO_ACTION_REQUIRED:
                return "Nie musisz nic robić";

            default:
                return "Domyślny tytuł powiadomienia";
        }
    }

    private String message(NotificationType notificationType) {
        switch (notificationType) {
            case MAKE_WAY_ON_ROAD:
                return "Pojazd uprzywilejowany nadjeżdża z tyłu";

            case MAKE_WAY_ON_JUNCTION:
                return "Pojazd uprzywilejowany zbliża się do skrzyżowania przed Tobą";

            case NO_ACTION_REQUIRED:
                return "Pojazd uprzywilejowany przejedzie w pobliżu, ale nie wpływasz na jego ruch";

            default:
                return "Domyślna treść powiadomienia";
        }
    }

    private int color(NotificationType notificationType) {
        switch (notificationType) {
            case MAKE_WAY_ON_ROAD:
                return mainActivity.getResources().getColor(R.color.colorNotificationMakeWayOnRoad);

            case MAKE_WAY_ON_JUNCTION:
                return mainActivity.getResources().getColor(R.color.colorNotificationMakeWayOnJunction);

            case NO_ACTION_REQUIRED:
                return mainActivity.getResources().getColor(R.color.colorNotificationNoActionRequired);

            default:
                return 0;
        }
    }
}
