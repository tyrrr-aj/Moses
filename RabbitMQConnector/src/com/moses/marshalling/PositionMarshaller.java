package com.moses.marshalling;

import com.moses.driverapp.backend.dto.GPSCoords;
import com.moses.position.Position;
import com.moses.position.PositionOnJunction;
import com.moses.position.PositionOnRoad;
import com.moses.driverapp.backend.dto.GPSCoords;

import java.nio.ByteBuffer;
import java.util.Arrays;

public class PositionMarshaller {
    public static byte[] marshallLocalizationUpdate(double latitude, double longitude) {
        return String.format("%d%d", gpsCoordToInt(latitude), gpsCoordToInt(longitude)).getBytes();
    }

    public static Position unmarshallPosition(byte[] messageBody) {
        if (new String(messageBody).equals("unknown")) {
            return null;
        }
        else if (new String(Arrays.copyOfRange(messageBody, 0, 8)).equals("junction")) {
            return new PositionOnJunction(new String(Arrays.copyOfRange(messageBody, 8, messageBody.length)));
        }
        else {
            double partOfRoad = ((float) (int) messageBody[0]) / 100;
            String roadId = new String(Arrays.copyOfRange(messageBody, 1, messageBody.length));

            return new PositionOnRoad(roadId, partOfRoad);
        }
    }

    public static GPSCoords unmarshallGPSCoords(byte[] rawCoords) {
        double lon = gpsCoordFromBytes(Arrays.copyOfRange(rawCoords, 0, 7));
        double lat = gpsCoordFromBytes(Arrays.copyOfRange(rawCoords, 7, 14));
        return new GPSCoords(lon, lat);
    }

    private static int gpsCoordToInt(double coord) {
        return (int) (coord * 100000);
    }

    private static double gpsCoordFromBytes(byte[] bytes) {
        return ((float) Integer.parseInt(new String(bytes)) / 100000);
    }

    private static int getInt(byte[] source) {
        return ByteBuffer.wrap(source).getShort();
    }

    private static byte[] getNBytes(int start, int n, byte[] source) {
        return Arrays.copyOfRange(source, start, start + n);
    }
}
