package com.moses;

import com.moses.driverapp.DriverAppConnector;
import com.moses.marshalling.NotificationMarshaller;
import com.moses.driverapp.Notification;
import com.moses.simulation.EVConnector;
import com.moses.simulation.EVType;

public class Main {

    public static void main(String[] args) {
    	// evConnectorTest();
		driverAppConnectionTest();
    }

    private static void driverAppConnectionTest() {
		try {
			DriverAppConnector connector = new DriverAppConnector();

			connector.listenForNotifications(message -> {
				System.out.println("Received notification!");
				System.out.printf("start=%f, end=%f, direction=%s, text=\"%s\"%n%n",
						message.beginAt,
						message.endAt,
						message.direction,
						message.text);
			});

			for (int i = 0; i < 3; i++) {
				Position position = connector.updateLocalization(50.06236, 19.93276);
				System.out.println("Current position is: " + position);
				Thread.sleep(1000);
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	public static void evConnectorTest() {
    	try {
			EVConnector connector = new EVConnector(EVType.AMBULANCE, "amb1");
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
