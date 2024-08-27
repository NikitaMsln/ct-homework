package hello;

import java.io.IOException;
import java.net.*;
import java.nio.charset.StandardCharsets;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public class HelloUDPClient implements HelloClient {
    private final static int TIMEOUT = 100;

    private static Runnable sendRequests(final int perThread, final String prefix, final InetAddress address, final int thread, final int port) {
        return () -> {
            DatagramSocket socket;
            try {
                socket = new DatagramSocket();
                socket.setSoTimeout(TIMEOUT);
            } catch (SocketException exception) {
                System.err.println("Unable to connect: " + exception);
                return;
            }

            byte[] buffer;
            try {
                buffer = new byte[socket.getReceiveBufferSize()];
            } catch (SocketException exception) {
                throw new IllegalStateException(exception);
            }
            DatagramPacket packet = new DatagramPacket(buffer, 0, address, port);
            for (int i = 1; i <= perThread; i++) {
                byte[] request = (prefix + thread + "_" + i).getBytes(StandardCharsets.UTF_8);

                boolean isEnded = false;
                while (!isEnded) {
                    try {
                        packet.setData(request);
                        packet.setLength(request.length);
                        socket.send(packet);

                        packet.setData(buffer);
                        packet.setLength(buffer.length);
                        socket.receive(packet);

                        String result = new String(buffer, 0, packet.getLength(), StandardCharsets.UTF_8);
                        if (Util.isCorrectClientAnswer(thread, i, result)) {
                            System.out.println(result);
                            isEnded = true;
                        }
                    } catch (SocketTimeoutException ignored) {
                    } catch (IOException exception) {
                        System.err.println("Socket exception: " + exception);
                    }
                }
            }
            socket.close();
        };
    }

    /**
     * Create a {@link HelloUDPClient} and run with {@code args[0]} host, {@code args[1]} port, {@code args[2]} request prefix, {@code args[3]} threads and {@code args[4]} requests per thread
     *
     * @param args arguments to run
     */
    public static void main(String[] args) {
        Util.mainClient(args, new HelloUDPClient());
    }

    @Override
    public void run(String host, int port, String prefix, int threads, int requests) {
        InetAddress address;
        try {
            address = InetAddress.getByName(host);
        } catch (UnknownHostException exception) {
            System.err.println("Incorrect host: " + exception);
            return;
        }

        ExecutorService executor = Executors.newThreadPerTaskExecutor(Executors.defaultThreadFactory());
        for (int i = 1; i <= threads; i++) {
            executor.execute(sendRequests(requests, prefix ,address, i, port));
        }
        executor.close();
    }
}
