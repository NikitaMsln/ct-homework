package expression;

public interface AllExpressions extends Expression, DoubleExpression, TripleExpression {
    int getPriority();
}
