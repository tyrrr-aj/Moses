package com.moses.app;

import androidx.annotation.RequiresApi;
import androidx.appcompat.app.AppCompatActivity;
import androidx.core.app.ActivityCompat;

import android.Manifest;
import android.content.Context;
import android.content.Intent;
import android.content.pm.PackageManager;
import android.graphics.Color;
import android.location.LocationManager;
import android.os.Build;
import android.os.Bundle;
import android.view.View;
import android.widget.Button;
import android.widget.EditText;

import com.ebanx.swipebtn.OnActiveListener;
import com.ebanx.swipebtn.SwipeButton;
import com.moses.RabbitMqConnector;
import com.moses.driverapp.backend.NotificationsReceiver;
import com.moses.driverapp.backend.interfaces.GPSAccessor;
import com.moses.driverapp.simulation.SimConnector;
import com.moses.driverapp.simulation.SimulatedGPSAccessor;
import com.moses.notifications.Notification;
import com.moses.notifications.NotificationType;

import java.io.IOException;
import java.util.concurrent.TimeoutException;


public class MainActivity extends AppCompatActivity{
    public MyLocationListener locationListener;
    private NotificationsReceiver notificationsReceiver;
    private SimConnector simConnector;

    public static String MESSAGE_TITLE = "com.moses.app.MESSAGE_TITLE";
    public static String MESSAGE_BODY = "com.moses.app.MESSAGE_BODY";
    public static String BACKGROUND_COLOR = "com.moses.app.BACKGROUND_COLOR";


    @RequiresApi(api = Build.VERSION_CODES.N)
    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
//        setUpLocationTracker();

        NotificationDisplayer displayer = new NotificationDisplayer(this);

        RabbitMqConnector rabbitMqConnector = null;
        GPSAccessor gpsAccessor = null;
        try {
            rabbitMqConnector = new RabbitMqConnector("192.168.0.6", "moses", "split");
            simConnector = new SimConnector(rabbitMqConnector);
            SimulatedGPSAccessor simGpsAccessor = new SimulatedGPSAccessor(simConnector);
            simGpsAccessor.init();
            gpsAccessor = simGpsAccessor;
        } catch (IOException e) {
            e.printStackTrace();
        }


        notificationsReceiver = new NotificationsReceiver(gpsAccessor, displayer, rabbitMqConnector);

        try {
            notificationsReceiver.receiveNotifications();
        } catch (IOException e) {
            e.printStackTrace();
        } catch (TimeoutException e) {
            e.printStackTrace();
        }

        setContentView(R.layout.activity_main);


        Button button = findViewById(R.id.button);
        EditText vehicleToTrackField = findViewById(R.id.vehicleToTrack);

        button.setOnClickListener(new View.OnClickListener(){
            @Override
            public void onClick(View view){
                simConnector.ensureConnected();
                simConnector.trackVehicle(vehicleToTrackField.getText().toString());
//                String longtitudeText = "Longtitude : ";
//                longtitudeText += locationListener.longitude;
//
//                String latitudeText = "Latitude : ";
//                latitudeText += locationListener.latitude;
//
//                String location = longtitudeText + "\n" + latitudeText;
//                textview.setText(location);
            }
        });

    }

    @Override
    protected void onDestroy() {
        super.onDestroy();
        notificationsReceiver.shutdown();
    }

    public void showMessage(String messageTitle, String messageBody, int backgroundColor) {
        Intent intent = new Intent(this, ShowNotification.class);
        intent.putExtra(MESSAGE_TITLE, messageTitle);
        intent.putExtra(MESSAGE_BODY, messageBody);
        intent.putExtra(BACKGROUND_COLOR, backgroundColor);
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