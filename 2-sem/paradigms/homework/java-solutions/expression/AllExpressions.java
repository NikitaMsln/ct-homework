package expression;

public interface AllExpressions extends TripleExpression, PriorityExpression {
    int evaluate(int x);
}
