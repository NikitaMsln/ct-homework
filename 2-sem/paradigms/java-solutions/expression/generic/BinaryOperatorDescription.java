package expression.generic;

import expression.ExpressionPriority;
import expression.OperatorExpression;

public enum BinaryOperatorDescription implements OperatorExpression {
    ADD(false, true, ExpressionPriority.ADDITIVE, "+"),
    DIVIDE(true, false, ExpressionPriority.MULTIPLICATIVE, "/"),
    MOD(false, false, ExpressionPriority.MULTIPLICATIVE, "mod"),
    MULTIPLY(false, true, ExpressionPriority.MULTIPLICATIVE, "*"),
    SUBTRACT(false, false, ExpressionPriority.ADDITIVE, "-");

    private final boolean isDependent;
    private final boolean isAssociative;
    private final ExpressionPriority priority;
    private final String name;

    BinaryOperatorDescription(boolean isDependent, boolean isAssociative, ExpressionPriority priority, String name) {
        this.isDependent = isDependent;
        this.isAssociative = isAssociative;
        this.priority = priority;
        this.name = name;
    }

    public boolean isAssociative() {
        return isAssociative;
    }

    public boolean isDependent() {
        return isDependent;
    }

    public int getPriority() {
        return priority.priority;
    }

    public String getName() {
        return name;
    }
}
