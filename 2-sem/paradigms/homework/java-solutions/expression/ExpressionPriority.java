package expression;

public enum ExpressionPriority {
    VALUE(100),
    UNARY(40),
    MULTIPLICATIVE(30),
    ADDITIVE(20),
    GCDLCM(10);

    public final int priority;

    ExpressionPriority(int priority) {
        this.priority = priority;
    }
}
