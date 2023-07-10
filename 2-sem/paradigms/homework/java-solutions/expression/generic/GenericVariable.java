package expression.generic;

import expression.ExpressionPriority;

import java.util.function.UnaryOperator;

public class GenericVariable<T> extends AbstractGenericExpression<T> {
    private final String name;
    private final UnaryOperator<T> operator;

    public GenericVariable(String name, UnaryOperator<T> operator) {
        this.name = name;
        this.operator = operator;
    }

    @Override
    public T evaluate(T x, T y, T z) {
        return operator.apply(switch (name) {
            case "x" -> x;
            case "y" -> y;
            case "z" -> z;
            default -> throw new IllegalArgumentException("Unexpected name of variable: " + name);
        });
    }

    @Override
    public T evaluate(T x) {
        return operator.apply(x);
    }

    @Override
    public int getPriority() {
        return ExpressionPriority.VALUE.priority;
    }

    @Override
    public boolean equals(Object that) {
        return (that instanceof GenericVariable<?>) && ((GenericVariable<?>) that).name.equals(this.name);
    }

    @Override
    public String toString() {
        return name;
    }

    @Override
    public String toMiniString() {
        return name;
    }
}
