package hello;

import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.SocketException;
import java.net.SocketTimeoutException;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.atomic.AtomicInteger;

public class HelloUDPServer implements NewHelloServer {
    private final static int TIMEOUT = 70;
    private final static int EXECUTOR_LIMIT_PER_THREAD = 1000;
    private ExecutorService receivers;
    private List<DatagramSocket> sockets;
    private ExecutorService senders;
    private AtomicInteger executorCounter;

    private static Runnable sendAnswer(final AtomicInteger counter, final DatagramSocket socket, final String format, final DatagramPacket packet) {
        return () -> {
            String value = new String(packet.getData(), 0, packet.getLength(), StandardCharsets.UTF_8);
            byte[] result = format.replaceAll("\\$", value).getBytes(StandardCharsets.UTF_8);
            packet.setData(result);
            packet.setLength(result.length);

            try {
                socket.send(packet);
            } catch (SocketException ignored) {
                return;
            } catch (IOException exception) {
                System.err.println("Send packet exception: " + exception);
            }
            counter.incrementAndGet();
        };
    }

    private static Runnable requestReader(final AtomicInteger counter, final ExecutorService executor, final DatagramSocket socket, final String format) {
        return () -> {
            int bufferSize;
            try {
                bufferSize = socket.getReceiveBufferSize();
            } catch (SocketException e) {
                throw new IllegalStateException(e);
            }
            byte[] buffer = new byte[bufferSize];
            DatagramPacket packet = new DatagramPacket(buffer, buffer.length);
            while (!Thread.currentThread().isInterrupted()) {
                try {
                    if (socket.isClosed()) {
                        return;
                    }
                    socket.receive(packet);

                    if (counter.decrementAndGet() > 0) {
                        executor.execute(sendAnswer(counter, socket, format, packet));
                        buffer = new byte[bufferSize];
                        packet = new DatagramPacket(buffer, buffer.length);
                    } else {
                        counter.incrementAndGet();
                    }

                } catch (SocketTimeoutException ignored) {
                } catch (SocketException ignored) {
                    return;
                } catch (IOException exception) {
                    System.err.println("Socket receive exception: " + exception);
                }
            }
        };
    }

    /**
     * Create a {@link HelloUDPServer} and start with {@code args[0]} port and {@code args[1]} threads.
     * Works until interrupt
     *
     * @param args arguments to start
     */
    public static void main(String[] args) {
        Util.mainServer(args, new HelloUDPServer());
    }

    @Override
    public void start(int threads, Map<Integer, String> ports) {
        close();
        if (ports.isEmpty()) {
            return;
        }

        senders = Executors.newFixedThreadPool(threads);
        sockets = new ArrayList<>();
        executorCounter = new AtomicInteger(EXECUTOR_LIMIT_PER_THREAD * threads);
        receivers = Executors.newThreadPerTaskExecutor(Executors.defaultThreadFactory());
        try {
            for (Map.Entry<Integer, String> entry : ports.entrySet()) {
                DatagramSocket socket = new DatagramSocket(entry.getKey());
                socket.setSoTimeout(TIMEOUT);
                sockets.add(socket);
                receivers.execute(new Thread(requestReader(executorCounter, senders, socket, entry.getValue())));
            }
        } catch (SocketException exception) {
            System.err.println("Unable to create server: " + exception);
            close();
        }
    }

    @Override
    public void close() {
        if (receivers == null) {
            return;
        }
        sockets.forEach(DatagramSocket::close);

        receivers.close();
        receivers = null;
        senders.close();
        sockets = null;
        senders = null;
        executorCounter = null;
    }
}
