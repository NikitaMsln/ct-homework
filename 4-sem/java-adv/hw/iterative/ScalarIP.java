package iterative;

import java.util.Comparator;
import java.util.List;
import java.util.function.Predicate;

/**
 * Scalar iterative parallelism support.
 *
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public interface ScalarIP {
    /**
     * Returns maximum value.
     *
     * @param threads number of concurrent threads.
     * @param values values to get maximum of.
     * @param comparator value comparator.
     * @param <T> value type.
     *
     * @return maximum of given values
     *
     * @throws InterruptedException if executing thread was interrupted.
     * @throws java.util.NoSuchElementException if no values are given.
     */
    <T> T maximum(int threads, List<? extends T> values, Comparator<? super T> comparator) throws InterruptedException;

    /**
     * Returns minimum value.
     *
     * @param threads number of concurrent threads.
     * @param values values to get minimum of.
     * @param comparator value comparator.
     * @param <T> value type.
     *
     * @return minimum of given values
     *
     * @throws InterruptedException if executing thread was interrupted.
     * @throws java.util.NoSuchElementException if no values are given.
     */
    <T> T minimum(int threads, List<? extends T> values, Comparator<? super T> comparator) throws InterruptedException;

    /**
     * Returns whether all values satisfy predicate.
     *
     * @param threads number of concurrent threads.
     * @param values values to test.
     * @param predicate test predicate.
     * @param <T> value type.
     *
     * @return whether all values satisfy predicate or {@code true}, if no values are given.
     *
     * @throws InterruptedException if executing thread was interrupted.
     */
    <T> boolean all(int threads, List<? extends T> values, Predicate<? super T> predicate) throws InterruptedException;

    /**
     * Returns whether any of values satisfies predicate.
     *
     * @param threads number of concurrent threads.
     * @param values values to test.
     * @param predicate test predicate.
     * @param <T> value type.
     *
     * @return whether any value satisfies predicate or {@code false}, if no values are given.
     *
     * @throws InterruptedException if executing thread was interrupted.
     */
    <T> boolean any(int threads, List<? extends T> values, Predicate<? super T> predicate) throws InterruptedException;

    /**
     * Returns number of values satisfying predicate.
     *
     * @param threads number of concurrent threads.
     * @param values values to test.
     * @param predicate test predicate.
     * @param <T> value type.
     *
     * @return number of values satisfying predicate.
     *
     * @throws InterruptedException if executing thread was interrupted.
     */
    <T> int count(int threads, List<? extends T> values, Predicate<? super T> predicate) throws InterruptedException;
}
