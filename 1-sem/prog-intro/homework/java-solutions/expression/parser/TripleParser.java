package expression.parser;

import expression.TripleExpression;

public interface TripleParser {
    TripleExpression parse(String expression);
}
