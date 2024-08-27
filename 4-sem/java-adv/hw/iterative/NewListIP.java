package iterative;

import java.util.List;
import java.util.function.Function;
import java.util.function.Predicate;

/**
 * List iterative parallelism support with sparse data.
 * <p>
 * Each method of this interface takes an extra positive integer {@code step}.
 * Only each {@code step}-th element of the input list should be used (counting from 0).
 *
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public interface NewListIP extends NewScalarIP, ListIP {
    /**
     * Join values to string.
     *
     * @param threads number of concurrent threads.
     * @param values values to join.
     * @param step step size.
     *
     * @return list of joined results of {@link Object#toString()} call on each value.
     *
     * @throws InterruptedException if executing thread was interrupted.
     */
    String join(int threads, List<?> values, int step) throws InterruptedException;

    /**
     * Filters values by predicate.
     *
     * @param threads number of concurrent threads.
     * @param values values to filter.
     * @param predicate filter predicate.
     * @param step step size.
     *
     * @return list of values satisfying given predicate. Order of values is preserved.
     *
     * @throws InterruptedException if executing thread was interrupted.
     */
    <T> List<T> filter(
            int threads,
            List<? extends T> values,
            Predicate<? super T> predicate,
            int step
    ) throws InterruptedException;

    /**
     * Maps values.
     *
     * @param threads number of concurrent threads.
     * @param values values to map.
     * @param f mapper function.
     * @param step step size.
     *
     * @return list of values mapped by given function.
     *
     * @throws InterruptedException if executing thread was interrupted.
     */
    <T, U> List<U> map(
            int threads,
            List<? extends T> values,
            Function<? super T, ? extends U> f,
            int step
    ) throws InterruptedException;

    @Override
    default String join(final int threads, final List<?> values) throws InterruptedException {
        return join(threads, values, 1);
    }

    @Override
    default <T> List<T> filter(
            final int threads,
            final List<? extends T> values,
            final Predicate<? super T> predicate
    ) throws InterruptedException {
        return filter(threads, values, predicate, 1);
    }

    @Override
    default <T, U> List<U> map(
            final int threads,
            final List<? extends T> values,
            final Function<? super T, ? extends U> f
    ) throws InterruptedException {
        return map(threads, values, f, 1);
    }
}
