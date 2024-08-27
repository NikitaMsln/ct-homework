package hello;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.nio.ByteBuffer;
import java.nio.channels.Channel;
import java.nio.channels.DatagramChannel;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

public class HelloUDPNonblockingClient implements HelloClient {
    private final static int TIMEOUT = 100;

    private static void sendRequest(SelectionKey key, String prefix) {
        try {
            if (key.isWritable()) {
                Context context = (Context) key.attachment();
                DatagramChannel channel = (DatagramChannel) key.channel();
                context.buffer.clear();
                context.buffer.put((prefix + context.thread + "_" + context.request).getBytes(StandardCharsets.UTF_8));
                context.buffer.flip();
                channel.send(context.buffer, channel.getRemoteAddress());
                key.interestOps(SelectionKey.OP_READ);
            }
        } catch (IOException exception) {
            System.err.println("Send request exception: " + exception);
        }
    }

    private static void receiveAnswer(SelectionKey key, int requests) {
        try {
            if (key.isReadable()) {
                Context context = (Context) key.attachment();
                DatagramChannel channel = (DatagramChannel) key.channel();
                context.buffer.clear();
                channel.receive(context.buffer);
                context.buffer.flip();
                String answer = StandardCharsets.UTF_8.decode(context.buffer).toString();
                if (Util.isCorrectClientAnswer(context.thread, context.request, answer)) {
                    System.out.println(answer);
                    context.request++;
                }
                if (context.request > requests) {
                    key.channel().close();
                } else {
                    key.interestOps(SelectionKey.OP_WRITE);
                }
            }
        } catch (IOException exception) {
            System.err.println("Receive answer exception: " + exception);
        }
    }

    /**
     * Create a {@link HelloUDPNonblockingClient} and run with {@code args[0]} host, {@code args[1]} port, {@code args[2]} request prefix, {@code args[3]} threads and {@code args[4]} requests per thread
     *
     * @param args arguments to run
     */
    public static void main(String[] args) {
        Util.mainClient(args, new HelloUDPNonblockingClient());
    }

    @Override
    public void run(String host, int port, String prefix, int threads, int requests) {
        Selector selector = null;
        List<Channel> channels = new ArrayList<>();
        int bufferSize = 0;
        try {
            selector = Selector.open();
            for (int i = 0; i < threads; i++) {
                DatagramChannel channel = DatagramChannel.open();
                channels.add(channel);
                channel.connect(new InetSocketAddress(host, port));
                if (bufferSize == 0) {
                    bufferSize = channel.socket().getReceiveBufferSize();
                }
                channel.configureBlocking(false);
                channel.register(selector, SelectionKey.OP_WRITE, new Context(i + 1, bufferSize));
            }
        } catch (IOException exception) {
            System.err.println("Creating selector error: " + exception);
            if (selector != null) {
                Util.nothrowClose(selector);
            }
            channels.forEach(Util::nothrowClose);
            return;
        }

        try {
            while (!selector.keys().isEmpty()) {
                selector.select(TIMEOUT);
                if (selector.selectedKeys().isEmpty()) {
                    for (SelectionKey key : selector.keys()) {
                        sendRequest(key, prefix);
                    }
                }

                for (Iterator<SelectionKey> it = selector.selectedKeys().iterator(); it.hasNext(); ) {
                    final SelectionKey key = it.next();

                    sendRequest(key, prefix);

                    receiveAnswer(key, requests);

                    it.remove();
                }
            }
        } catch (IOException exception) {
            System.err.println("Selector exception: " + exception);
        } finally {
            Util.nothrowClose(selector);
            channels.forEach(Util::nothrowClose);
        }
    }

    private static class Context {
        public final int thread;
        public int request = 1;
        public ByteBuffer buffer;

        public Context(int thread, int bufferSize) {
            this.thread = thread;
            buffer = ByteBuffer.allocate(bufferSize);
        }
    }
}
