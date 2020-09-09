package com.moses.app;

import androidx.appcompat.app.AppCompatActivity;

import android.content.Intent;
import android.os.Bundle;

public class ShowNotification extends AppCompatActivity {

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_show_notification);

        Intent intent = getIntent();
        String message = intent.getStringExtra(MainActivity.MESSAGE_BODY);
    }
}