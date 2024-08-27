import java.io.Closeable;
import java.io.IOException;
import java.net.InetSocketAddress;
import java.nio.ByteBuffer;
import java.nio.channels.Channel;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.nio.channels.SocketChannel;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.concurrent.*;

public class TCPProxyServer implements Closeable {
    private static final int THREAD_COUNT = 10;
    private static final int BUFFER_LIMIT = 100;
    private static final int TIMEOUT = 300;
    private static final int QUEUE_POLL_TIMEOUT = 10;
    private ExecutorService redirections;
    private List<Selector> selectors;
    private List<Channel> channels;

    private static void nothrowClose(final Closeable closeable) {
        try {
            closeable.close();
        } catch (final IOException ignored) {
        }
    }

    private static void redirect(final Selector selector) {
        while (!Thread.currentThread().isInterrupted() && !selector.keys().isEmpty()) {
            try {
                selector.select(TIMEOUT);
                for (final Iterator<SelectionKey> it = selector.selectedKeys().iterator(); it.hasNext(); ) {
                    final SelectionKey key;
                    key = it.next();
                    if (key.isReadable()) {
                        final Context context = (Context) key.attachment();
                        final SocketChannel channel = (SocketChannel) key.channel();
                        // :NOTE: * Блокирующее действие, a write мог бы освободить
                        final ByteBuffer buffer = context.freeBuffers.poll(QUEUE_POLL_TIMEOUT, TimeUnit.MILLISECONDS);

                        if (buffer != null) {
                            buffer.clear();
                            channel.read(buffer);
                            buffer.flip();
                            context.out.put(buffer);
                        }
                    }
                    if (key.isWritable()) {
                        final Context context = (Context) key.attachment();
                        final SocketChannel channel = (SocketChannel) key.channel();
                        while (!context.in.isEmpty()) {
                            // :NOTE: - Аналогично
                            final ByteBuffer buffer = context.in.poll(QUEUE_POLL_TIMEOUT, TimeUnit.MILLISECONDS);
                            if (buffer == null) {
                                break;
                            }
                            // :NOTE: * Нельзя писать несколько в цикле
                            // :NOTE: - Нет дозаписи, если ушло не всё
                            channel.write(buffer);
                            context.freeBuffers.put(buffer);
                        }
                        // :NOTE: * Не получится отправить несколько пакетов с ответами
                        key.interestOps(SelectionKey.OP_READ);
                    }
                    it.remove();
                }
            } catch (final InterruptedException e) {
                return;
            } catch (final IOException exception) {
                System.err.println("Selector exception: " + exception);
                return;
            }
        }
    }

    public void run(final Map<Integer, InetSocketAddress> addresses) throws IOException {
        close();
        redirections = Executors.newThreadPerTaskExecutor(Executors.defaultThreadFactory());

        // :NOTE: * Много селекторов
        selectors = new ArrayList<>(THREAD_COUNT);
        channels = new ArrayList<>();
        int selector;
        try {
            for (selector = 0; selector < THREAD_COUNT; selector++) {
                selectors.add(Selector.open());
            }
            selector = 0;
            int bufferSize = -1;
            for (final Map.Entry<Integer, InetSocketAddress> entry : addresses.entrySet()) {
                final SocketChannel local = SocketChannel.open();
                channels.add(local);
                local.configureBlocking(false);
                local.connect(new InetSocketAddress(entry.getKey()));

                final SocketChannel remote = SocketChannel.open();
                channels.add(remote);
                remote.configureBlocking(false);
                remote.connect(entry.getValue()); // :NOTE: * connect без finishConnect

                if (bufferSize < 0) {
                    bufferSize = local.socket().getReceiveBufferSize();
                }

                // :NOTE: * Использование LinkedBlockingQueue
                final BlockingQueue<ByteBuffer> toLocal = new LinkedBlockingQueue<>();
                final BlockingQueue<ByteBuffer> toRemote = new LinkedBlockingQueue<>();
                final BlockingQueue<ByteBuffer> freeBuffers = new LinkedBlockingQueue<>();
                for (int i = 0; i < BUFFER_LIMIT; i++) {
                    freeBuffers.put(ByteBuffer.allocate(bufferSize));
                }

                local.register(selectors.get(selector), SelectionKey.OP_READ | SelectionKey.OP_WRITE, new Context(toRemote, toLocal, freeBuffers));
                selector = (selector + 1) % THREAD_COUNT;
                remote.register(selectors.get(selector), SelectionKey.OP_READ | SelectionKey.OP_WRITE, new Context(toLocal, toRemote, freeBuffers));
                selector = (selector + 1) % THREAD_COUNT;
            }
        } catch (final InterruptedException exception) {
            close();
            Thread.currentThread().interrupt();
        } catch (final IOException exception) {
            close();
            throw exception;
        }

        selectors.forEach(select -> redirections.execute(() -> redirect(select)));
    }

    public void close() {
        if (redirections == null) {
            return;
        }
        channels.forEach(TCPProxyServer::nothrowClose);
        selectors.forEach(TCPProxyServer::nothrowClose);
        redirections.close();
        redirections = null;
        channels = null;
        selectors = null;
    }

    private static class Context {
        public BlockingQueue<ByteBuffer> out;
        public BlockingQueue<ByteBuffer> in;
        public BlockingQueue<ByteBuffer> freeBuffers;

        public Context(final BlockingQueue<ByteBuffer> out, final BlockingQueue<ByteBuffer> in, final BlockingQueue<ByteBuffer> freeBuffers) {
            this.out = out;
            this.in = in;
            this.freeBuffers = freeBuffers;
        }
    }
}
