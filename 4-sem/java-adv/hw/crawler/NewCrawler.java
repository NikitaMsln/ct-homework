package crawler;

import java.util.Set;

/**
 * Crawls websites, filtering <a href="http://tools.ietf.org/html/rfc3986">URLs</a> by substrings.
 *
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public interface NewCrawler extends Crawler {
    /**
     * Downloads website up to specified depth.
     *
     * @param url start URL.
     * @param depth download depth.
     * @param excludes URLs containing one of given substrings are ignored.
     * @return download result.
     */
    Result download(String url, int depth, Set<String> excludes);

    @Override
    default Result download(final String url, final int depth) {
        return download(url, depth, Set.of());
    }
}
