import java.io.*;
import java.net.InetSocketAddress;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Main {
    private final static Pattern LINE_PATTERN = Pattern.compile("^(?<local>\\p{IsDigit}+) (?<host>(.+)) (?<port>\\p{IsDigit}+)$");

    public static void main(final String[] args) {
        if (args.length < 1 || args[0] == null) {
            System.err.println("Enter file with table");
            return;
        }

        final Map<Integer, InetSocketAddress> redirectTable;
        try {
            redirectTable = parseFile(args[0]);
        } catch (final IOException exception) {
            System.err.println("Cannot read table: " + exception);
            return;
        }

        if (redirectTable.isEmpty()) {
            System.err.println("Not found any one correct address");
            return;
        }
        final TCPProxyServer server = new TCPProxyServer();
        try {
            server.run(redirectTable);
        } catch (final IOException exception) {
            System.err.println("Unable to run proxy server: " + exception);
        }
        final BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
        try {
            reader.readLine();
        } catch (final IOException ignored) {
        }
        server.close();
    }

    private static Map<Integer, InetSocketAddress> parseFile(final String filename) throws IOException {
        final Map<Integer, InetSocketAddress> result = new HashMap<>();
        final LineNumberReader reader = new LineNumberReader(new FileReader(filename, StandardCharsets.UTF_8));

        String value;
        while ((value = reader.readLine()) != null) {
            final Matcher matcher = LINE_PATTERN.matcher(value);
            if (matcher.find()) {
                final int local;
                try {
                    local = Integer.parseInt(matcher.group("local"));
                } catch (final NumberFormatException exception) {
                    System.err.println("WARNING: Invalid local port (" + matcher.group("local") + "):" + exception);
                    // :NOTE: - Почему-то чтение прекращается
                    break;
                }
                final int port;
                try {
                    port = Integer.parseInt(matcher.group("port"));
                } catch (final NumberFormatException exception) {
                    System.err.println("WARNING: Invalid remote port (" + matcher.group("port") + "):" + exception);
                    break;
                }
                final InetSocketAddress address = new InetSocketAddress(matcher.group("host"), port);
                if (address.isUnresolved()) {
                    System.err.println("WARNING: Invalid remote address (" + matcher.group("host") + ":" + port + ")");
                    break;
                }
                result.put(local, address);
            } else {
                System.err.println("WARNING: Incorrect line format: " + reader.getLineNumber() + ") " + "\"" + value + "\"");
            }
        }

        return result;
    }
}
