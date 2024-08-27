package hello;

import java.io.Closeable;
import java.io.IOException;
import java.util.Arrays;
import java.util.Objects;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Util {
    private final static int SLEEP_TIME = 1000;
    private final static Pattern CORRECT_CLIENT_ANSWER_PATTERN = Pattern.compile("^Hello, (.*[^{\\p{IsDigit}}])?(?<thread>\\p{IsDigit}+)[^{\\p{IsDigit}}]+(?<request>\\p{IsDigit}+)$");

    /**
     * Check that {@code value} starts with "Hello, " and last 2 numbers in it are {@code thread} and {@code request}
     *
     * @param thread pre-last number in string
     * @param request last number in string
     * @param value checked string
     * @return correctness of string
     */
    public static boolean isCorrectClientAnswer(int thread, int request, String value) {
        Matcher matcher = CORRECT_CLIENT_ANSWER_PATTERN.matcher(value);
        if (matcher.find()) {
            try {
                int threadFound = Integer.parseInt(matcher.group("thread"));
                int requestFound = Integer.parseInt(matcher.group("request"));
                return threadFound == thread && requestFound == request;
            } catch (NumberFormatException exception) {
                return false;
            }
        }
        return false;
    }

    /**
     * Run client with parameters in string format in args
     *
     * @param args parameters to run
     * @param client running client
     */
    public static void mainClient(String[] args, HelloClient client) {
        if (args == null || args.length < 5) {
            System.err.println("Must be 5 arguments: name or ip of server, port, request prefix, number of threads, number of request per thread");
            return;
        }
        if (Arrays.stream(args).limit(5).anyMatch(Objects::isNull)) {
            System.err.println("Arguments can't be null");
            return;
        }

        int port;
        if ((port = parseArgument(args[1], "Port")) < 0) {
            return;
        }

        int threads;
        if ((threads = parseArgument(args[3], "Threads count")) < 0) {
            return;
        }

        int perThread;
        if ((perThread = parseArgument(args[4], "Requests per thread count")) < 0) {
            return;
        }

        client.run(args[0], port, args[2], threads, perThread);
    }

    /**
     * Start server with parameters in string format in args until it's interrupted
     *
     * @param args parameters to run
     * @param server running server
     */
    public static void mainServer(String[] args, HelloServer server) {
        if (args == null || args.length < 2) {
            System.err.println("Must be 2 arguments: port, number of threads");
            return;
        }
        if (Arrays.stream(args).limit(2).anyMatch(Objects::isNull)) {
            System.err.println("Arguments can't be null");
            return;
        }

        int port;
        if ((port = parseArgument(args[0], "Port")) < 0) {
            return;
        }

        int threads;
        if ((threads = parseArgument(args[1], "Threads count")) < 0) {
            return;
        }

        try (server) {
            server.start(port, threads);
            while (!Thread.currentThread().isInterrupted()) {
                Thread.sleep(SLEEP_TIME);
            }
        } catch (InterruptedException ignored) {
            server.close();
        }
    }

    /**
     * Close {@code closeable} and ignore all {@link IOException}
     *
     * @param closeable
     */
    public static void nothrowClose(Closeable closeable) {
        try {
            closeable.close();
        } catch (IOException ignored) {
        }
    }

    private static int parseArgument(String arg, String name) {
        try {
            return Integer.parseInt(arg);
        } catch (NumberFormatException exception) {
            System.err.println(name + " isn't a number: " + exception);
            return -1;
        }
    }
}
