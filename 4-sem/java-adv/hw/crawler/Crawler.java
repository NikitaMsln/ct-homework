package crawler;

/**
 * Crawls websites.
 *
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public interface Crawler extends AutoCloseable {
    /**
     * Downloads website up to specified depth.
     *
     * @param url start <a href="http://tools.ietf.org/html/rfc3986">URL</a>.
     * @param depth download depth.
     * @return download result.
     */
    Result download(String url, int depth);

    /**
     * Closes this crawler, freeing any allocated resources.
     */
    @Override
    void close();
}
