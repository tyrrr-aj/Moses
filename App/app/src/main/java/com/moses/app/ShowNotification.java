package com.moses.app;

import android.app.Activity;
import android.content.Intent;
import android.os.Bundle;
import android.view.View;
import android.widget.TextView;

import com.ebanx.swipebtn.OnActiveListener;
import com.ebanx.swipebtn.SwipeButton;

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

        setActivityBackgroundColor(intent.getIntExtra(MainActivity.BACKGROUND_COLOR, 0));

        SwipeButton confirmButton = findViewById(R.id.confirm_button);
        confirmButton.setOnActiveListener(new OnActiveListener() {
            @Override
            public void onActive() {
                finish();
            }
        });
    }

    public void setActivityBackgroundColor(int color) {
        View view = this.getWindow().getDecorView();
        view.setBackgroundColor(color);
    }
}