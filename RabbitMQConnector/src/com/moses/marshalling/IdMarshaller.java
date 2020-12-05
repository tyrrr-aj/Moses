package com.moses.marshalling;

public class IdMarshaller {
    public static byte[] marshallId(String id) {
        return id.getBytes();
    }

    public static String unmarshallId(byte[] id) {
        return new String(id);
    }
}
