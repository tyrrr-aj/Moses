package com.moses.app;

import androidx.appcompat.app.AppCompatActivity;
import androidx.core.app.ActivityCompat;

import android.Manifest;
import android.content.Context;
import android.content.Intent;
import android.content.pm.PackageManager;
import android.location.LocationListener;
import android.location.LocationManager;
import android.os.Bundle;
import android.view.View;
import android.widget.Button;
import android.widget.TextView;

public class MainActivity extends AppCompatActivity{
    public static String MESSAGE_BODY = "com.moses.app.MESSAGE_BODY";

    public MyLocationListener locationListener;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setUpLocationTracker();
        RabbitMQConnector connector = new RabbitMQConnector();
        connector.subscribe(this);

        setContentView(R.layout.activity_main);

        Button button = (Button) findViewById(R.id.button);
        TextView textview = (TextView) findViewById(R.id.textView);

        button.setOnClickListener(new View.OnClickListener(){
            @Override
            public void onClick(View view){
                String longtitudeText = "Longtitude : ";
                longtitudeText += locationListener.longitude;

                String latitudeText = "Latitude : ";
                latitudeText += locationListener.latitude;

                String location = longtitudeText + "\n" + latitudeText;
                textview.setText(location);
            }
        });
    }

    public void showMessage(String message) {
        Intent intent = new Intent(this, ShowNotification.class);
        intent.putExtra(MESSAGE_BODY, message);
        startActivity(intent);
    }

    public void setUpLocationTracker() {
        LocationManager locationManager = (LocationManager) getSystemService(Context.LOCATION_SERVICE);
        locationListener = new MyLocationListener();
        if (ActivityCompat.checkSelfPermission(this, Manifest.permission.ACCESS_FINE_LOCATION) != PackageManager.PERMISSION_GRANTED && ActivityCompat.checkSelfPermission(this, Manifest.permission.ACCESS_COARSE_LOCATION) != PackageManager.PERMISSION_GRANTED) {
            // TODO: Consider calling
            //    ActivityCompat#requestPermissions
            // here to request the missing permissions, and then overriding
            //   public void onRequestPermissionsResult(int requestCode, String[] permissions,
            //                                          int[] grantResults)
            // to handle the case where the user grants the permission. See the documentation
            // for ActivityCompat#requestPermissions for more details.
            return;
        }
        locationManager.requestLocationUpdates(
                LocationManager.GPS_PROVIDER, 5000, 10, locationListener);
    }
}