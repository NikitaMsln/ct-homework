package iterative;

import java.util.List;
import java.util.function.BinaryOperator;
import java.util.function.Function;

/**
 * Advanced iterative parallelism support with sparse data.
 * <p>
 * Each method of this interface takes an extra positive integer {@code step}.
 * Only each {@code step}-th element of the input list should be used (counting from 0).
 *
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public interface AdvancedIP extends NewListIP {
    /**
     * Reduces values using monoid.
     *
     * @param threads number of concurrent threads.
     * @param values values to reduce.
     * @param identity monoid identity element.
     * @param operator monoid operation.
     * @param step step size.
     *
     * @return values reduced by provided monoid or {@code identity} if no values specified.
     *
     * @throws InterruptedException if executing thread was interrupted.
     */
    <T> T reduce(
            int threads,
            List<T> values,
            T identity,
            BinaryOperator<T> operator,
            int step
    ) throws InterruptedException;

    /**
     * Maps and reduces values using monoid.
     *
     * @param threads number of concurrent threads.
     * @param values values to reduce.
     * @param lift mapping function.
     * @param identity monoid identity element.
     * @param operator monoid operation.
     * @param step step size.
     *
     * @return values reduced by provided monoid or {@code identity} if no values specified.
     *
     * @throws InterruptedException if executing thread was interrupted.
     */
    <T, R> R mapReduce(
            int threads,
            List<T> values,
            Function<T, R> lift,
            R identity,
            BinaryOperator<R> operator,
            int step
    ) throws InterruptedException;
}
