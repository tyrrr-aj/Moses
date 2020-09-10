package com.moses.app;

import androidx.appcompat.app.AppCompatActivity;

import android.content.Intent;
import android.os.Bundle;
import android.widget.TextView;

public class ShowNotification extends AppCompatActivity {

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_show_notification);

        Intent intent = getIntent();
        String message = intent.getStringExtra(MainActivity.MESSAGE_BODY);

        TextView notificationBody = findViewById(R.id.notification_body);
        notificationBody.setText(message);
    }
}