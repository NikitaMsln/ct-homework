package implementor;

import java.nio.file.Path;

/**
 * Builds jar-file containing compiled implementation generated by {@link Impler}.
 *
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public interface JarImpler extends Impler {
    /**
     * Produces <var>.jar</var> file implementing class or interface specified by provided <var>token</var>.
     * <p>
     * Generated class' name should be the same as the class name of the type token with <var>Impl</var> suffix
     * added.
     *
     * @param token type token to create implementation for.
     * @param jarFile target <var>.jar</var> file.
     * @throws ImplerException when implementation cannot be generated.
     */
    void implementJar(Class<?> token, Path jarFile) throws ImplerException;
}
