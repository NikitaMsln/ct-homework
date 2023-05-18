package expression;

public interface AllExpressions extends TripleExpression, ToMiniString, PriorityExpression {
    int evaluate(int x);
}
