package crawler;

import java.util.List;

/**
 * Crawls websites, filtering URLs by host.
 *
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public interface AdvancedCrawler extends NewCrawler {
    /**
     * Downloads website up to specified depth.
     *
     * @param url start <a href="http://tools.ietf.org/html/rfc3986">URL</a>.
     * @param depth download depth.
     * @param hosts domains to follow, pages on another domains should be ignored.
     * @return download result.
     */
    Result advancedDownload(String url, int depth, List<String> hosts);
}
