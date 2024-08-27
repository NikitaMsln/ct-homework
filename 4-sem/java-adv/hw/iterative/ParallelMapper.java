package iterative;

import java.util.List;
import java.util.function.Function;

/**
 * Maps function over lists.
 *
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public interface ParallelMapper extends AutoCloseable {
    /**
     * Maps function {@code f} over specified {@code items}.
     * Mapping for each item is performed in parallel.
     *
     * @throws InterruptedException if calling thread was interrupted
     */
    <T, R> List<R> map(Function<? super T, ? extends R> f, List<? extends T> items) throws InterruptedException;

    /**
     * Stops all threads.
     * <p>
     * Easy version: all unfinished mappings are left in undefined state.
     * </p>
     * <p>
     *     Hard version: all unfinished mappings should throw exception.
     * </p>
     * */
    @Override
    void close();
}
