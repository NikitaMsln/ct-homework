package expression.generic;

import expression.ExpressionPriority;

import java.util.function.UnaryOperator;

public class GenericUnaryExpression<T> extends AbstractGenericExpression<T> {
    private final GenericExpression<T> value;
    private final UnaryOperator<T> operator;
    private final String name;

    public GenericUnaryExpression(GenericExpression<T> value, UnaryOperator<T> operator, String name) {
        this.value = value;
        this.operator = operator;
        this.name = name;
    }

    @Override
    public T evaluate(T x) {
        return operator.apply(value.evaluate(x));
    }

    @Override
    public T evaluate(T x, T y, T z) {
        return operator.apply(value.evaluate(x, y, z));
    }

    @Override
    public boolean equals(Object that) {
        return (that instanceof GenericUnaryExpression<?>) &&
                ((GenericUnaryExpression<?>) that).value.equals(this.value) &&
                that.getClass() == this.getClass();
    }

    @Override
    public int getPriority() {
        return ExpressionPriority.UNARY.priority;
    }

    @Override
    public String toString() {
        return name + "(" + value.toString() + ")";
    }

    @Override
    public String toMiniString() {
        if (value.getPriority() >= this.getPriority()) {
            return name + " " + value.toMiniString();
        } else {
            return name + "(" + value.toMiniString() + ")";
        }
    }
}
