package walk;

import java.io.IOException;
import java.io.InputStream;
import java.math.BigInteger;
import java.security.MessageDigest;
import java.util.Arrays;

public class SHAHashEvaluator implements HashEvaluator<InputStream> {
    private static final int BUFFER_SIZE = 1 << 12;
    private final MessageDigest digest;

    SHAHashEvaluator(MessageDigest messageDigest) {
        digest = messageDigest;
        digest.reset();
    }

    @Override
    public byte[] hash(InputStream value) {
        byte[] emptyResult = new byte[40];
        Arrays.fill(emptyResult, (byte) 0);
        if (value == null) {
            return emptyResult;
        }

        byte[] buffer = new byte[BUFFER_SIZE];
        int buffer_size;
        try {
            while ((buffer_size = value.read(buffer, 0, BUFFER_SIZE)) >= 0) {
                digest.update(buffer, 0, buffer_size);
            }
        } catch (IOException exception) {
            digest.reset();
            return emptyResult;
        }
        return digest.digest();
    }

    @Override
    public String formattedHash(InputStream value) {
        return String.format("%040x", new BigInteger(1, hash(value)));
    }
}

