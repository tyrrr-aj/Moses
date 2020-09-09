package com.moses.app;

import androidx.appcompat.app.AppCompatActivity;

import android.content.Intent;
import android.os.Bundle;

public class MainActivity extends AppCompatActivity {
    public static String MESSAGE_BODY = "com.moses.app.MESSAGE_BODY";

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        RabbitMQConnector connector = new RabbitMQConnector();
        connector.subscribe(this);

        setContentView(R.layout.activity_main);
    }

    public void showMessage(String message) {
        Intent intent = new Intent(this, ShowNotification.class);
        intent.putExtra(MESSAGE_BODY, message);
        startActivity(intent);
    }
}