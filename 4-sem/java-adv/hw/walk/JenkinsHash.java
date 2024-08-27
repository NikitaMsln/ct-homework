package walk;

import java.io.IOException;
import java.io.InputStream;
import java.math.BigInteger;

public class JenkinsHash implements HashEvaluator<InputStream> {
    private static final int BUFFER_SIZE = 1 << 12;

    @Override
    public byte[] hash(InputStream value) {
        return BigInteger.valueOf(evalHash(value) & 0xFFFFFFFFL).toByteArray();
    }

    @Override
    public String formattedHash(InputStream value) {
        return String.format("%08x", evalHash(value) & 0xFFFFFFFFL);
    }

    private int evalHash(InputStream value) {
        if (value == null) {
            return 0;
        }

        byte[] buffer = new byte[BUFFER_SIZE];
        int buffer_size;
        int hash = 0;
        try {
            while ((buffer_size = value.read(buffer, 0, BUFFER_SIZE)) >= 0) {
                for (int i = 0; i < buffer_size; i++) {
                    hash += (buffer[i] & 0xFF);
                    hash += hash << 10;
                    hash ^= hash >>> 6;
                }
            }
        } catch (IOException exception) {
            return 0;
        }

        hash += hash << 3;
        hash ^= hash >>> 11;
        hash += hash << 15;
        return hash;
    }
}

