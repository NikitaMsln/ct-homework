package hello;

/**
 * Client interface for {@link HelloClientTest}.
 *
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public interface HelloClient {
    /**
     * Runs Hello client.
     * This method should return when all requests are completed.
     *
     * @param host server host
     * @param port server port
     * @param prefix request prefix
     * @param threads number of request threads
     * @param requests number of requests per thread.
     */
    void run(String host, int port, String prefix, int threads, int requests);
}
