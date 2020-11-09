package com.moses.marshalling;

import com.moses.Position;

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
        double partOfRoad = ((float) (int) messageBody[0]) / 100;
        String roadId = new String(Arrays.copyOfRange(messageBody, 1, messageBody.length));

        return new Position(roadId, partOfRoad);
    }

    private static int gpsCoordToInt(double coord) {
        return (int) (coord * 100000);
    }

    private static int getInt(byte[] source) {
        return ByteBuffer.wrap(source).getShort();
    }

    private static byte[] getNBytes(int start, int n, byte[] source) {
        return Arrays.copyOfRange(source, start, start + n);
    }
}
