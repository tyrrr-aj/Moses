package com.moses.app;

import androidx.appcompat.app.AppCompatActivity;

import android.app.Activity;
import android.content.Intent;
import android.os.Bundle;
import android.widget.TextView;

public class ShowNotification extends Activity {

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_show_notification);

        Intent intent = getIntent();
        String messageBody = intent.getStringExtra(MainActivity.MESSAGE_BODY);
        String messageTitle = intent.getStringExtra(MainActivity.MESSAGE_TITLE);

        TextView notificationTitle = findViewById(R.id.notification_title);
        notificationTitle.setText(messageTitle);

        TextView notificationBody = findViewById(R.id.notification_body);
        notificationBody.setText(messageBody);

    }
}