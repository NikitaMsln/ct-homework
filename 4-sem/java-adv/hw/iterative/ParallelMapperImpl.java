package iterative;

import java.util.*;
import java.util.function.Function;

public class ParallelMapperImpl implements ParallelMapper {
    private final List<Thread> threadList;
    private final Queue<Runnable> runnable = new LinkedList<>();
    private final boolean[] isAlive = {true};

    /**
     * Create the parallel mapper with {@code threads} resolving threads
     *
     * @param threads number of threads
     */
    public ParallelMapperImpl(int threads) {
        threadList = new ArrayList<>(threads);
        Runnable threadRunnable = this::threadRun;
        for (int i = 0; i < threads; i++) {
            threadList.add(new Thread(threadRunnable));
            threadList.getLast().start();
        }
    }

    private void threadRun() {
        while (!Thread.currentThread().isInterrupted()) {
            Runnable currentTask;
            synchronized (runnable) {
                while ((currentTask = runnable.poll()) == null) {
                    try {
                        runnable.wait();
                    } catch (InterruptedException exception) {
                        Thread.currentThread().interrupt();
                        return;
                    }
                }
            }
            currentTask.run();
        }
    }

    @Override
    public <T, R> List<R> map(Function<? super T, ? extends R> f, List<? extends T> items) throws InterruptedException {
        synchronized (isAlive) {
            if (!isAlive[0]) {
                throw new IllegalStateException("Threads already were interrupted");
            }
        }
        List<R> result = new ArrayList<>(Collections.nCopies(items.size(), null));
        final int[] completed = {0};
        final RuntimeException[] functionCallException = {null};
        for (int i = 0; i < items.size(); i++) {
            final int currentElem = i;
            synchronized (runnable) {
                runnable.add(
                        () -> {
                            try {
                                result.set(currentElem, f.apply(items.get(currentElem)));
                            } catch (RuntimeException exception) {
                                synchronized (functionCallException) {
                                    if (functionCallException[0] == null) {
                                        functionCallException[0] = exception;
                                    } else {
                                        functionCallException[0].addSuppressed(exception);
                                    }
                                }
                            }
                            synchronized (completed) {
                                completed[0]++;
                                completed.notify();
                            }
                        }
                );
                runnable.notify();
            }
        }
        synchronized (completed) {
            while (completed[0] != items.size()) {
                synchronized (isAlive) {
                    if (!isAlive[0]) {
                        throw new IllegalStateException("Threads were interrupted before resolving");
                    }
                }
                completed.wait();
            }
        }
        if (functionCallException[0] != null) {
            throw functionCallException[0];
        }
        return result;
    }

    @Override
    public void close() {
        synchronized (isAlive) {
            isAlive[0] = false;
        }
        threadList.forEach(Thread::interrupt);
        boolean mustInterrupted = false;
        for (int thread = 0; thread < threadList.size();) {
            try {
                threadList.get(thread).join();
                thread++;
            } catch (InterruptedException ignored) {
                mustInterrupted = true;
            }
        }
        if (mustInterrupted) {
            Thread.currentThread().interrupt();
        }
    }
}
