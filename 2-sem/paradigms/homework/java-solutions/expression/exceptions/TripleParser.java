package expression.exceptions;

import expression.TripleExpression;

public interface TripleParser {
    TripleExpression parse(String expression) throws ParsingException;
}
