package com.moses;

import com.moses.driverapp.backend.NotificationsReceiver;
import com.moses.driverapp.backend.dto.GPSCoords;
import com.moses.driverapp.backend.interfaces.Displayer;
import com.moses.driverapp.backend.interfaces.GPSAccessor;
import com.moses.driverapp.simulation.SimConnector;
import com.moses.driverapp.simulation.SimulatedGPSAccessor;
import com.moses.notifications.Notification;
import com.moses.simulation.AppConnector;
import com.moses.simulation.EVConnector;
import com.moses.simulation.EVType;

import java.io.IOException;
import java.util.concurrent.TimeoutException;

public class Main {

    public static void main(String[] args) throws IOException, TimeoutException {
//    	evConnectorTest();
		driverAppConnectionTest();
//		simulationAppConnectionTest();
	}

    private static void simulationAppConnectionTest() {
		SimulatedGPSAccessor gpsAccessor = null;
		SimConnector simConnector = null;
		RabbitMqConnector appRabbitConnector;

		try {
			appRabbitConnector = new RabbitMqConnector("localhost", "guest", "guest");
			simConnector = new SimConnector(appRabbitConnector);
			gpsAccessor = new SimulatedGPSAccessor(simConnector);
		} catch (IOException | TimeoutException e) {
			System.err.println("[ERROR - APP] Creation of SimConnector and/or SimulatedGPSAccessor failed");
			e.printStackTrace();
		}

		AppConnector appConnector = null;
		RabbitMqConnector simRabbitConnector;

		try {
			simRabbitConnector = new RabbitMqConnector("localhost", "guest", "guest");
			appConnector = new AppConnector(simRabbitConnector);
		} catch (TimeoutException | IOException e) {
			System.err.println("[ERROR - SIM] Creation of AppConnector failed");
			e.printStackTrace();
		}

		try {
			appConnector.startTracking();
		} catch (IOException e) {
			e.printStackTrace();
		}

		try {
			simConnector.trackVehicle("test_vehicle");
		} catch (IOException e) {
			System.err.println("[ERROR _ APP] Turning on tracking specific vehicle failed");
			e.printStackTrace();
		}

		try {
			appConnector.updateVehiclePosition("test_vehicle", 19.12345, 50.98765);
		} catch (IOException e) {
			System.err.println("[ERROR - SIM] Turning on tracking specific vehicle failed");
			e.printStackTrace();
		}

		while (gpsAccessor.getCurrentCoords() == null) {
			System.out.println(gpsAccessor.getCurrentCoords());
			try {
				Thread.sleep(1000);
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
		}
		System.out.println(gpsAccessor.getCurrentCoords());

		try {
			simConnector.trackVehicle("another_test_vehicle");
		} catch (IOException e) {
			System.err.println("[ERROR - APP] Changing tracked vehicle failed");
			e.printStackTrace();
		}

		try {
			appConnector.updateVehiclePosition("another_test_vehicle", 21.00001, 55.00001);
		} catch (IOException e) {
			System.err.println("[ERROR - SIM] Turning on tracking specific vehicle failed");
			e.printStackTrace();
		}

		for (int i = 0; i < 2; i++) {
			System.out.println(gpsAccessor.getCurrentCoords());
			try {
				Thread.sleep(1000);
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
		}
		System.out.println(gpsAccessor.getCurrentCoords());

		try {
			simConnector.stopListening();
		} catch (IOException e) {
			e.printStackTrace();
		} catch (TimeoutException e) {
			e.printStackTrace();
		}
		try {
			appConnector.stop();
		} catch (IOException e) {
			e.printStackTrace();
		} catch (TimeoutException e) {
			e.printStackTrace();
		}
	}

    private static void driverAppConnectionTest() {
		GPSAccessor gpsAccessor = new GPSAccessor() {
			@Override
			public GPSCoords getCurrentCoords() {
				return new GPSCoords(50.05853,19.92589);
			}
		};
		Displayer displayer = new Displayer() {
			@Override
			public void displayNotification(Notification notification) {
				System.out.println(notification.text);
			}
		};

		NotificationsReceiver receiver = new NotificationsReceiver(gpsAccessor, displayer);

		try {
			receiver.receiveNotifications();
		} catch (IOException e) {
			e.printStackTrace();
		} catch (TimeoutException e) {
			e.printStackTrace();
		}
	}

	public static void evConnectorTest() {
    	try {
			EVConnector connector = new EVConnector(EVType.AMBULANCE.name(), "amb1");
			connector.sendDispatchMessageAsync(50.06236, 19.93276);

			System.out.println("Dispatch message sent");

			for (int i = 0; i < 3; i++) {
				Thread.sleep(1000);
				connector.sendTrackingMessageAsync(50.10000, 20.00000);
				System.out.println("Tracking message sent");
			}

		} catch (Exception e) {
			e.printStackTrace();
		}
	}
}
