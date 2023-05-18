package expression.generic;

import expression.exceptions.ParsingException;

public interface Tabulator {
    Object[][][] tabulate(final String mode, final String expression, final int x1, final int x2, final int y1, final int y2, final int z1, final int z2) throws ParsingException;
}
