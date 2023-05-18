package expression.generic;

import expression.exceptions.EvaluateException;
import expression.exceptions.ParsingException;
import expression.generic.generators.*;

import java.util.Map;

public class GenericTabulator implements Tabulator {
    private static final Map<String, ExpressionGenerator<?>> TYPES = Map.of(
            "i", new CheckedIntegerGenerator(),
            "u", new UncheckedIntegerGenerator(),
            "p", new ModIntegerGenerator(),
            "d", new DoubleGenerator(),
            "s", new ShortGenerator(),
            "bi", new BigIntegerGenerator()
    );
    private static class Tabulator<T> {
        private GenericExpression<T> expression;
        private final ExpressionGenerator<T> generator;

        Tabulator(final ExpressionGenerator<T> generator) {
            this.generator = generator;
        }

        public void setExpression(final String expression) throws ParsingException {
            this.expression = new ExpressionParser<>(generator).parse(expression);
        }

        public Object evaluate(final int x, final int y, final int z) throws EvaluateException {
            if (expression == null) {
                throw new EvaluateException("Expression not found");
            }
            return expression.evaluate(generator.valueOf(x), generator.valueOf(y), generator.valueOf(z));
        }
    }

    @Override
    public Object[][][] tabulate(final String mode, final String expression, final int x1, final int x2, final int y1, final int y2, final int z1, final int z2) throws ParsingException {
        Tabulator<?> tabulator;
        if (TYPES.containsKey(mode)) {
            tabulator = new Tabulator<>(TYPES.get(mode));
        } else {
            throw new ParsingException("Mode \"" + mode + "\" is undefined");
        }

        tabulator.setExpression(expression);

        final Object[][][] result = new Object[x2 - x1 + 1][y2 - y1 + 1][z2 - z1 + 1];
        for (int x = x1; x <= x2; x++) {
            for (int y = y1; y <= y2; y++) {
                for (int z = z1; z <= z2; z++) {
                    try {
                        result[x - x1][y - y1][z - z1] = tabulator.evaluate(x, y, z);
                    } catch (final EvaluateException | ArithmeticException e) { // :NOTE: |
                        // :NOTE: result[x - x1][y - y1][z - z1] = null;
                    }
                }
            }
        }
        return result;
    }
}
