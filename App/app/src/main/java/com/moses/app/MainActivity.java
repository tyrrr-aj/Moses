package com.moses.app;

import androidx.annotation.RequiresApi;
import androidx.appcompat.app.AppCompatActivity;
import androidx.core.app.ActivityCompat;

import android.Manifest;
import android.content.Context;
import android.content.Intent;
import android.content.pm.PackageManager;
import android.location.LocationManager;
import android.os.Build;
import android.os.Bundle;

import com.moses.RabbitMqConnector;
import com.moses.driverapp.backend.NotificationsReceiver;
import com.moses.driverapp.backend.interfaces.GPSAccessor;
import com.moses.driverapp.simulation.SimConnector;
import com.moses.driverapp.simulation.SimulatedGPSAccessor;

import java.io.IOException;
import java.util.concurrent.TimeoutException;


public class MainActivity extends AppCompatActivity{
    public MyLocationListener locationListener;
    private NotificationsReceiver notificationsReceiver;

    public static String MESSAGE_BODY = "com.moses.app.MESSAGE_BODY";

    @RequiresApi(api = Build.VERSION_CODES.N)
    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
//        setUpLocationTracker();

        NotificationDisplayer displayer = new NotificationDisplayer(this);

        RabbitMqConnector rabbitMqConnector;
        GPSAccessor gpsAccessor = null;
        try {
            rabbitMqConnector = new RabbitMqConnector("10.0.2.2", "moses", "split");
            SimConnector simConnector = new SimConnector(rabbitMqConnector);
            gpsAccessor = new SimulatedGPSAccessor(simConnector);
        } catch (IOException e) {
            e.printStackTrace();
        }


        notificationsReceiver = new NotificationsReceiver(gpsAccessor, displayer);

        try {
            notificationsReceiver.receiveNotifications();
        } catch (IOException e) {
            e.printStackTrace();
        } catch (TimeoutException e) {
            e.printStackTrace();
        }

        setContentView(R.layout.activity_main);


//        Button button = (Button) findViewById(R.id.button);
////        TextView textview = (TextView) findViewById(R.id.textView);
//
//        button.setOnClickListener(new View.OnClickListener(){
//            @Override
//            public void onClick(View view){
////                String longtitudeText = "Longtitude : ";
////                longtitudeText += locationListener.longitude;
////
////                String latitudeText = "Latitude : ";
////                latitudeText += locationListener.latitude;
////
////                String location = longtitudeText + "\n" + latitudeText;
////                textview.setText(location);
//                try {
//                    Thread thread = new Thread(() -> {
//                        try {
//                            connector.sendLocalization(50.24535, 19.43256);
//                        } catch (IOException e) {
//                            e.printStackTrace();
//                        }
//                    });
//                    thread.start();
//                } catch (Exception e) {
//                    e.printStackTrace();
//                }
//            }
//        });
    }

    @Override
    protected void onDestroy() {
        super.onDestroy();
        notificationsReceiver.shutdown();
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

//    private void updateLocalizationView(Localization localization) {
//        TextView textView = findViewById(R.id.localizationText);
//        textView.setText(String.format("RoadId: %s\nPartOfRoad: %f\nDirection: %s", localization.RoadId, localization.PartOfRoad, localization.direction));
//    }
}