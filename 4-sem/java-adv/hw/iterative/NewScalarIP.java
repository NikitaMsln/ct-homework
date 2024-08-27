package iterative;

import java.util.Comparator;
import java.util.List;
import java.util.function.Predicate;

/**
 * Scalar iterative parallelism support with sparse data.
 * <p>
 * Each method of this interface takes an extra positive integer {@code step}.
 * Only each {@code step}-th element of the input list should be used (counting from 0).
 *
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public interface NewScalarIP extends ScalarIP {
    /**
     * Returns maximum value.
     *
     * @param threads number of concurrent threads.
     * @param values values to get maximum of.
     * @param comparator value comparator.
     * @param step step size.
     * @param <T> value type.
     *
     * @return maximum of given values
     *
     * @throws InterruptedException if executing thread was interrupted.
     * @throws java.util.NoSuchElementException if no values are given.
     */
    <T> T maximum(int threads, List<? extends T> values, Comparator<? super T> comparator, int step) throws InterruptedException;

    /**
     * Returns minimum value.
     *
     * @param threads number of concurrent threads.
     * @param values values to get minimum of.
     * @param comparator value comparator.
     * @param step step size.
     * @param <T> value type.
     *
     * @return minimum of given values
     *
     * @throws InterruptedException if executing thread was interrupted.
     * @throws java.util.NoSuchElementException if no values are given.
     */
    <T> T minimum(int threads, List<? extends T> values, Comparator<? super T> comparator, int step) throws InterruptedException;

    /**
     * Returns whether all values satisfy predicate.
     *
     * @param threads number of concurrent threads.
     * @param values values to test.
     * @param predicate test predicate.
     * @param step step size.
     * @param <T> value type.
     *
     * @return whether all values satisfy predicate or {@code true}, if no values are given.
     *
     * @throws InterruptedException if executing thread was interrupted.
     */
    <T> boolean all(int threads, List<? extends T> values, Predicate<? super T> predicate, int step) throws InterruptedException;

    /**
     * Returns whether any of values satisfies predicate.
     *
     * @param threads number of concurrent threads.
     * @param values values to test.
     * @param predicate test predicate.
     * @param step step size.
     * @param <T> value type.
     *
     * @return whether any value satisfies predicate or {@code false}, if no values are given.
     *
     * @throws InterruptedException if executing thread was interrupted.
     */
    <T> boolean any(int threads, List<? extends T> values, Predicate<? super T> predicate, int step) throws InterruptedException;

    /**
     * Returns number of values satisfying predicate.
     *
     * @param threads number of concurrent threads.
     * @param values values to test.
     * @param predicate test predicate.
     * @param step step size.
     * @param <T> value type.
     *
     * @return number of values satisfying predicate.
     *
     * @throws InterruptedException if executing thread was interrupted.
     */
    <T> int count(int threads, List<? extends T> values, Predicate<? super T> predicate, int step) throws InterruptedException;

    @Override
    default <T> T maximum(
            final int threads,
            final List<? extends T> values,
            final Comparator<? super T> comparator
    ) throws InterruptedException {
        return maximum(threads, values, comparator, 1);
    }

    @Override
    default <T> T minimum(
            final int threads,
            final List<? extends T> values,
            final Comparator<? super T> comparator
    ) throws InterruptedException {
        return minimum(threads, values, comparator, 1);
    }

    @Override
    default <T> boolean all(
            final int threads,
            final List<? extends T> values,
            final Predicate<? super T> predicate
    ) throws InterruptedException {
        return all(threads, values, predicate, 1);
    }

    @Override
    default <T> boolean any(
            final int threads,
            final List<? extends T> values,
            final Predicate<? super T> predicate
    ) throws InterruptedException {
        return any(threads, values, predicate, 1);
    }

    @Override
    default <T> int count(
            final int threads,
            final List<? extends T> values,
            final Predicate<? super T> predicate
    ) throws InterruptedException {
        return count(threads, values, predicate, 1);
    }
}
