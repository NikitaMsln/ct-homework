package crawler;

import java.io.IOException;
import java.net.MalformedURLException;
import java.util.*;
import java.util.concurrent.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.BiPredicate;
import java.util.stream.Collectors;

public class WebCrawler implements AdvancedCrawler {
    private static class DownloadContext {
        public final Set<String> downloadedUrls = new HashSet<>();
        public final ConcurrentMap<String, IOException> failedUrls = new ConcurrentHashMap<>();
        public final Set<String> visitedUrls = new HashSet<>();
        public final ConcurrentMap<String, Queue<String>> nextDownload = new ConcurrentHashMap<>();
        public final AtomicInteger downloadCounter = new AtomicInteger(1);
        public final AtomicInteger exctractCounter = new AtomicInteger(0);
        public final Object notifyer = new Object();

        private Runnable downloadRun(final String host, final String url, final int depth, final BiPredicate<String, String> filter,
                                     final WebCrawler webCrawler
        ) {
            return () -> webCrawler.download(host, url, depth, filter, this);
        }
    }

    private final Downloader downloader;
    private final ConcurrentMap<String, Semaphore> downloaderPerHost = new ConcurrentHashMap<>();
    private final ExecutorService downloadExecutor;
    private final ExecutorService extractExecutor;
    private final int perHost;

    /**
     * Create a parallel crawler with {@code downloader}, {@code downloaders} number of download threads,
     * {@code extractors} number of extract threads and  {@code perHost} number of maximum parallel request to one host
     *
     * @param downloader url downloader
     * @param downloaders number of download threads
     * @param extractors number of extract threads
     * @param perHost number of maximum parallel request to one host
     */
    public WebCrawler(final Downloader downloader, final int downloaders, final int extractors, final int perHost) {
        this.downloader = downloader;
        this.perHost = perHost;
        downloadExecutor = Executors.newFixedThreadPool(downloaders);
        extractExecutor = Executors.newFixedThreadPool(extractors);
    }

    @Override
    public Result download(final String url, final int depth, final Set<String> excludes) {
        return download(url, depth, (host, link) -> excludes.stream().noneMatch(link::contains));
    }

    @Override
    public Result download(final String url, final int depth) {
        return download(url, depth, (host, link) -> true);
    }

    @Override
    public Result advancedDownload(final String url, final int depth, final List<String> hosts) {
        final Set<String> hostsSet = hosts.stream().collect(Collectors.toUnmodifiableSet());
        return download(url, depth, (host, link) -> hostsSet.contains(host));
    }

    @Override
    public void close() {
        downloadExecutor.close();
        extractExecutor.close();
    }

    private Result download(final String url, int depth, final BiPredicate<String, String> filter) {
        final String host;
        try {
            host = URLUtils.getHost(url);
        } catch (final MalformedURLException exception) {
            return new Result(List.of(), Map.of(url, exception));
        }
        if (!filter.test(host, url)) {
            return new Result(List.of(), Map.of());
        }
        final DownloadContext context = new DownloadContext();
        depth--;
        downloadExecutor.execute(context.downloadRun(host, url, depth, filter, this));
        while (true) {
            try {
                synchronized (context.notifyer) {
                    context.notifyer.wait();
                }
            } catch (final InterruptedException exception) {
                close();
                Thread.currentThread().interrupt();
                return null;
            }
            if (context.downloadCounter.get() == 0 && context.exctractCounter.get() == 0) {
                depth--;
                if (context.nextDownload.isEmpty()) {
                    break;
                }
                synchronized (context.nextDownload) {
                    context.downloadCounter.set(
                            context.nextDownload.values().stream()
                                    .map(Collection::size)
                                    .reduce(0, Integer::sum)
                    );
                    while (!context.nextDownload.isEmpty()) {
                        for (final Map.Entry<String, Queue<String>> entry : context.nextDownload.entrySet()) {
                            if (entry.getValue().isEmpty()) {
                                context.nextDownload.remove(entry.getKey());
                            } else {
                                downloadExecutor.execute(context.downloadRun(entry.getKey(), entry.getValue().poll(), depth, filter,
                                                                             this
                                ));
                            }
                        }
                    }
                }
            }
        }
        return new Result(new ArrayList<>(context.downloadedUrls), context.failedUrls);
    }

    private void download(
            final String host,
            final String url,
            final int depth,
            final BiPredicate<String, String> filter,
            final DownloadContext context
    ) {
        // :NOTE: concurrency utilities
        synchronized (context.visitedUrls) {
            if (context.visitedUrls.contains(url)) {
                if (context.downloadCounter.decrementAndGet() == 0 && context.exctractCounter.get() == 0) {
                    synchronized (context.notifyer) {
                        context.notifyer.notify();
                    }
                }
                return;
            }
            context.visitedUrls.add(url);
        }
        try {
            downloaderPerHost.putIfAbsent(host, new Semaphore(perHost));
            downloaderPerHost.get(host).acquire(); // :NOTE: useless wait
            final Document result = downloader.download(url);
            if (depth > 0) {
                extractExecutor.execute(extractRun(result, filter, context));
            }
            downloaderPerHost.get(host).release();
            synchronized (context.downloadedUrls) {
                context.downloadedUrls.add(url);
            }
        } catch (final IOException exception) {
            downloaderPerHost.get(host).release();
            synchronized (context.failedUrls) {
                context.failedUrls.merge(url, exception, (x, y) -> {
                    x.addSuppressed(y);
                    return x;
                });
            }
        } catch (final InterruptedException exception) {
            Thread.currentThread().interrupt();
        }
        if (context.downloadCounter.decrementAndGet() == 0 && context.exctractCounter.get() == 0) {
            synchronized (context.notifyer) {
                context.notifyer.notify();
            }
        }
    }

    private Runnable extractRun(final Document document, final BiPredicate<String, String> filter, final DownloadContext context) {
        context.exctractCounter.incrementAndGet();
        return () -> {
            try {
                final List<String> links = document.extractLinks();
                for (final String link : links) {
                    synchronized (context.visitedUrls) {
                        if (context.visitedUrls.contains(link)) {
                            continue;
                        }
                    }
                    try {
                        final String host = URLUtils.getHost(link);
                        if (!filter.test(host, link)) {
                            continue;
                        }
                        synchronized (context.nextDownload) {
                            context.nextDownload.putIfAbsent(host, new ArrayDeque<>());
                            context.nextDownload.get(host).add(link);
                        }
                    } catch (final MalformedURLException exception) {
                        synchronized (context.failedUrls) {
                            context.failedUrls.merge(link, exception, (x, y) -> {
                                x.addSuppressed(y);
                                return x;
                            });
                        }
                    }
                }
            } catch (final IOException exception) {
                synchronized (context.failedUrls) {
                    context.failedUrls.merge(document.toString(), exception, (x, y) -> {
                        x.addSuppressed(y);
                        return x;
                    });
                }
            }
            if (context.exctractCounter.decrementAndGet() == 0) {
                synchronized (context.notifyer) {
                    context.notifyer.notify();
                }
            }
        };
    }

    /**
     * Create a {@link WebCrawler} with {@link CachingDownloader} and walk by {@code args[0]} url, if args has more than 2 elements
     * set {@code args[1]} - depth, {@code args[2]} - downloaders, {@code args[3]} - extractors, {@code args[4]} - perHost
     *
     * @param args arguments to create {@link WebCrawler} and download links
     */
    public static void main(final String[] args) {
        if (args == null || args.length < 1 || args[0] == null) {
            System.err.println("Entry url to download in first argument");
            return;
        }
        int depth = 1;
        int downloaders = 1;
        int extractors = 1;
        int perHost = 1;
        try {
            // :NOTE: copy-paste
            if (args.length > 1 && args[1] != null) {
                depth = Integer.parseInt(args[1]);
            }
            if (args.length > 2 && args[2] != null) {
                downloaders = Integer.parseInt(args[2]);
            }
            if (args.length > 3 && args[3] != null) {
                extractors = Integer.parseInt(args[3]);
            }
            if (args.length > 4 && args[4] != null) {
                perHost = Integer.parseInt(args[4]);
            }
        } catch (final NumberFormatException exception) {
            System.err.println("Warning: " + exception);
            System.err.printf("Selected values: depth = %d, downloaders = %d, extractors = %d, perHost = %d%n", depth, downloaders, extractors, perHost);
        }
        final Downloader downloader;
        try {
            downloader = new CachingDownloader(0.1);
        } catch (final IOException exception) {
            System.err.println("Downloader init error: " + exception);
            return;
        }
        final Crawler crawler = new WebCrawler(downloader, downloaders, extractors, perHost);
        final Result result = crawler.download(args[1], depth);
        crawler.close();
        System.out.println("Downloaded links:");
        for (final String link : result.getDownloaded()) {
            System.out.println(link);
        }
        System.out.println("Errors:");
        for (final Map.Entry<String, IOException> entry : result.getErrors().entrySet()) {
            System.out.println(entry.getKey() + ": " + entry.getValue().toString());
        }
    }
}

