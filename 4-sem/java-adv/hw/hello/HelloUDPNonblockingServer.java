package hello;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.SocketAddress;
import java.nio.ByteBuffer;
import java.nio.channels.*;
import java.nio.charset.StandardCharsets;
import java.util.*;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.LinkedBlockingQueue;

public class HelloUDPNonblockingServer implements NewHelloServer {
    private final static int TIMEOUT = 70;
    private final static int EXECUTOR_LIMIT_PER_THREAD = 50;

    private List<Channel> channels;
    private ExecutorService selectorManager;
    private ExecutorService workers;

    private static Runnable generateAnswer(ByteBuffer buffer, SocketAddress address, Context context, SelectionKey key) {
        return () -> {
            String request = StandardCharsets.UTF_8.decode(buffer).toString();
            buffer.clear();
            buffer.put(context.format.replaceAll("\\$", request).getBytes());
            buffer.flip();
            try {
                context.answers.put(Map.entry(buffer, address));
                key.interestOpsOr(SelectionKey.OP_WRITE);
                key.selector().wakeup();
            } catch (InterruptedException | CancelledKeyException ignored) {
            }
        };
    }

    private static Runnable manageSelector(Selector selector, ExecutorService workers) {
        return () -> {
            while (!Thread.currentThread().isInterrupted() && !selector.keys().isEmpty()) {
                try {
                    selector.select(TIMEOUT);
                    for (Iterator<SelectionKey> it = selector.selectedKeys().iterator(); it.hasNext(); ) {
                        SelectionKey key;
                        key = it.next();
                        try {
                            if (key.isReadable()) {
                                Context context = (Context) key.attachment();
                                DatagramChannel channel = (DatagramChannel) key.channel();
                                ByteBuffer buffer;
                                synchronized (context.buffers) {
                                    if (context.buffers.isEmpty()) {
                                        buffer = null;
                                    } else {
                                        buffer = context.buffers.poll();
                                    }
                                }
                                if (buffer != null) {
                                    buffer.clear();
                                    SocketAddress address = channel.receive(buffer);
                                    buffer.flip();
                                    workers.execute(generateAnswer(buffer, address, context, key));
                                }
                            }
                            if (key.isWritable()) {
                                Context context = (Context) key.attachment();
                                DatagramChannel channel = (DatagramChannel) key.channel();
                                while (!context.answers.isEmpty()) {
                                    Map.Entry<ByteBuffer, SocketAddress> answer = context.answers.take();
                                    channel.send(answer.getKey(), answer.getValue());
                                    synchronized (context.buffers) {
                                        context.buffers.add(answer.getKey());
                                    }
                                }
                                key.interestOps(SelectionKey.OP_READ);
                            }
                        } catch (CancelledKeyException ignored) {
                        }
                        it.remove();
                    }
                } catch (ClosedChannelException ignored) {
                } catch (InterruptedException e) {
                    return;
                } catch (IOException exception) {
                    System.err.println("Selector exception: " + exception);
                    return;
                }
            }
            Util.nothrowClose(selector);
        };
    }

    /**
     * Create a {@link HelloUDPNonblockingServer} and start with {@code args[0]} port and {@code args[1]} threads.
     * Works until interrupt
     *
     * @param args arguments to start
     */
    public static void main(String[] args) {
        Util.mainServer(args, new HelloUDPNonblockingServer());
    }

    @Override
    public void start(int threads, Map<Integer, String> ports) {
        if (ports.isEmpty()) {
            return;
        }
        close();
        Selector selector = null;
        workers = Executors.newFixedThreadPool(threads);
        selectorManager = Executors.newThreadPerTaskExecutor(Executors.defaultThreadFactory());
        channels = new ArrayList<>();
        int bufferSize = 0;
        try {
            selector = Selector.open();
            for (Map.Entry<Integer, String> entry : ports.entrySet()) {
                DatagramChannel channel = DatagramChannel.open();
                channels.add(channel);
                InetSocketAddress address = new InetSocketAddress(entry.getKey());
                channel.bind(address);
                if (bufferSize == 0) {
                    bufferSize = channel.socket().getReceiveBufferSize();
                }
                channel.configureBlocking(false);
                channel.register(selector, SelectionKey.OP_READ, new Context(entry.getValue(), address, bufferSize));
            }
            selectorManager.execute(manageSelector(selector, workers));
        } catch (IOException exception) {
            System.err.println("Starting selector or channels error: " + exception);
            if (selector != null) {
                Util.nothrowClose(selector);
            }
            close();
        }
    }

    @Override
    public void close() {
        if (channels == null) {
            return;
        }
        channels.forEach(Util::nothrowClose);
        workers.close();
        selectorManager.close();
        channels = null;
        workers = null;
        selectorManager = null;
    }

    private static class Context {
        public final String format;
        public final InetSocketAddress address;
        public final BlockingQueue<Map.Entry<ByteBuffer, SocketAddress>> answers = new LinkedBlockingQueue<>();
        public final Queue<ByteBuffer> buffers = new ArrayDeque<>();

        public Context(String format, InetSocketAddress address, int bufferSize) {
            this.format = format;
            this.address = address;
            synchronized (buffers) {
                for (int i = 0; i < EXECUTOR_LIMIT_PER_THREAD; i++) {
                    buffers.add(ByteBuffer.allocate(bufferSize));
                }
            }
        }
    }
}
