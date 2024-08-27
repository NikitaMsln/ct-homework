package hello;

import java.util.Map;

/**
 * Multi-port {@link HelloServer}.
 * Per-port response format is specified as string, where each {@code $} symbol should
 * be replaced by request.
 *
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public interface NewHelloServer extends HelloServer {
    /**
     * Starts a new Hello server.
     * This method should return immediately.
     *
     * @param threads number of working threads.
     * @param ports port no to response format mapping.
     */
    void start(int threads, Map<Integer, String> ports);

    default void start(final int port, final int threads) {
        start(threads, Map.of(port, "Hello, $"));
    }
}
