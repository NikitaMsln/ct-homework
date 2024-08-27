package iterative;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.function.BinaryOperator;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.IntStream;
import java.util.stream.Stream;

public class IterativeParallelism implements AdvancedIP {
    private static class ConcatenateLinkedList<T> {
        private class Node {
            public Node next = this;
            public Node prev = this;
            public final T value;

            Node(T element) {
                value = element;
            }
        }

        private void link(Node first, Node second) {
            first.next = second;
            second.prev = first;
        }

        private final Node root = new Node(null);
        public ConcatenateLinkedList(T element) {
            link(root, new Node(element));
            link(root.next, root);
        }

        public ConcatenateLinkedList() {
            link(root, root);
        }

        public ConcatenateLinkedList<T> concat(ConcatenateLinkedList<T> other) {
            ConcatenateLinkedList<T> result = new ConcatenateLinkedList<>();
            if (root.next != root) {
                link(result.root, root.next);
                link(root.prev, result.root);
                link(root, root);
            }
            if (other.root.next != other.root) {
                link(result.root.prev, other.root.next);
                link(other.root.prev, result.root);
                link(other.root, other.root);
            }
            return result;
        }

        public List<T> toList() {
            List<T> result = new ArrayList<>();
            for (Node curr = root.next; curr != root; curr = curr.next) {
                result.add(curr.value);
            }
            return result;
        }
    }

    private final ParallelMapper mapper;

    /**
     * Create IterativeParallelism which create a new threads
     */
    public IterativeParallelism() {
        mapper = null;
    }


    /**
     * Create IterativeParallelism which use parallel mapper and don't create threads
     */
    public IterativeParallelism(ParallelMapper parallelMapper) {
        mapper = parallelMapper;
    }

    private <T> List<Stream<T>> getItemsPerThread(int threads, List<T> items, int step) {
        List<T> filteredList = IntStream.range(0, (items.size() + step - 1) / step)
                .mapToObj(i -> items.get(i * step))
                .toList();
        int elementsPerThread = (filteredList.size() + threads - 1) / threads + 1;
        return IntStream.range(0, threads)
                .mapToObj(i -> filteredList.subList(
                        Integer.min(filteredList.size(), i * elementsPerThread),
                        Integer.min(filteredList.size(), (i + 1) * elementsPerThread)
                        ).stream()
                ).toList();
    }

    private <T, R> R streamMapReduce(int threads, List<T> values, Function<Stream<T>, R> lift, R identity, BinaryOperator<R> operator, int step) throws InterruptedException {
        List<Thread> threadList = new ArrayList<>(threads);
        List<Stream<T>> splitItems = getItemsPerThread(threads, values, step);
        if (mapper == null) {
            List<R> threadResults = new ArrayList<>(Collections.nCopies(threads, identity));
            for (int thread = 0; thread < threads; thread++) {
                int finalThread = thread;
                threadList.add(new Thread(
                        () -> threadResults.set(finalThread, lift.apply(splitItems.get(finalThread)))
                ));
                threadList.getLast().start();
            }
            InterruptedException exception = null;
            for (Thread thread : threadList) {
                try {
                    thread.join();
                } catch (InterruptedException interruptedException) {
                    thread.interrupt();
                    if (exception == null) {
                        exception = interruptedException;
                    } else {
                        exception.addSuppressed(interruptedException);
                    }
                }
            }
            if (exception != null) {
                throw exception;
            }
            return threadResults.stream().reduce(identity, operator);
        } else {
            return mapper.map(lift, splitItems).stream().reduce(identity, operator);
        }
    }

    @Override
    public String join(int threads, List<?> values, int step) throws InterruptedException {
        return String.join("", mapReduce(
                threads,
                values,
                x -> new ConcatenateLinkedList<>(x.toString()),
                new ConcatenateLinkedList<String>(),
                ConcatenateLinkedList::concat,
                step
        ).toList());
    }

    @Override
    public <T> List<T> filter(int threads, List<? extends T> values, Predicate<? super T> predicate, int step) throws InterruptedException {
        return mapReduce(
                threads,
                values,
                x -> {
                    if (predicate.test(x)) {
                        return new ConcatenateLinkedList<T>(x);
                    } else {
                        return new ConcatenateLinkedList<T>();
                    }
                },
                new ConcatenateLinkedList<T>(),
                ConcatenateLinkedList::concat,
                step
        ).toList();
    }

    @Override
    public <T, U> List<U> map(int threads, List<? extends T> values, Function<? super T, ? extends U> f, int step) throws InterruptedException {
        return mapReduce(
                threads,
                values,
                x -> new ConcatenateLinkedList<U>(f.apply(x)),
                new ConcatenateLinkedList<U>(),
                ConcatenateLinkedList::concat,
                step
        ).toList();
    }

    @Override
    public String join(int threads, List<?> values) throws InterruptedException {
        return join(threads, values, 1);
    }

    @Override
    public <T> List<T> filter(int threads, List<? extends T> values, Predicate<? super T> predicate) throws InterruptedException {
        return filter(threads, values, predicate, 1);
    }

    @Override
    public <T, U> List<U> map(int threads, List<? extends T> values, Function<? super T, ? extends U> f) throws InterruptedException {
        return map(threads, values, f, 1);
    }

    @Override
    public <T> T maximum(int threads, List<? extends T> values, Comparator<? super T> comparator, int step) throws InterruptedException {
        return streamMapReduce(
                threads,
                values,
                stream -> stream.max(comparator).orElse(null),
                null,
                (x, y) -> {
                    if (x == null) {
                        return y;
                    } else if (y == null) {
                        return x;
                    } else if (comparator.compare(x, y) >= 0) {
                        return x;
                    } else {
                        return y;
                    }
                },
                step
        );
    }

    @Override
    public <T> T minimum(int threads, List<? extends T> values, Comparator<? super T> comparator, int step) throws InterruptedException {
        return maximum(threads, values, comparator.reversed(), step);
    }

    @Override
    public <T> boolean all(int threads, List<? extends T> values, Predicate<? super T> predicate, int step) throws InterruptedException {
        return !any(threads, values, predicate.negate(), step);
    }

    @Override
    public <T> boolean any(int threads, List<? extends T> values, Predicate<? super T> predicate, int step) throws InterruptedException {
        return streamMapReduce(
                threads,
                values,
                stream -> stream.anyMatch(predicate),
                false,
                Boolean::logicalOr,
                step
        );
    }

    @Override
    public <T> int count(int threads, List<? extends T> values, Predicate<? super T> predicate, int step) throws InterruptedException {
        return mapReduce(
                threads,
                values,
                value -> predicate.test(value)? 1 : 0,
                0,
                Integer::sum,
                step
        );
    }

    @Override
    public <T> T maximum(int threads, List<? extends T> values, Comparator<? super T> comparator) throws InterruptedException {
        return maximum(threads, values, comparator, 1);
    }

    @Override
    public <T> T minimum(int threads, List<? extends T> values, Comparator<? super T> comparator) throws InterruptedException {
        return minimum(threads, values, comparator, 1);
    }

    @Override
    public <T> boolean all(int threads, List<? extends T> values, Predicate<? super T> predicate) throws InterruptedException {
        return all(threads, values, predicate, 1);
    }

    @Override
    public <T> boolean any(int threads, List<? extends T> values, Predicate<? super T> predicate) throws InterruptedException {
        return any(threads, values, predicate, 1);
    }

    @Override
    public <T> int count(int threads, List<? extends T> values, Predicate<? super T> predicate) throws InterruptedException {
        return count(threads, values, predicate, 1);
    }

    @Override
    public <T> T reduce(int threads, List<T> values, T identity, BinaryOperator<T> operator, int step) throws InterruptedException {
        return mapReduce(threads, values, Function.identity(), identity, operator, step);
    }

    @Override
    public <T, R> R mapReduce(int threads, List<T> values, Function<T, R> lift, R identity, BinaryOperator<R> operator, int step) throws InterruptedException {
        return streamMapReduce(
                threads,
                values,
                stream -> stream.map(lift).reduce(identity, operator),
                identity,
                operator,
                step
                );
    }
}
