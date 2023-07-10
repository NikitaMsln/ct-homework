package expression.generic;

import expression.ExpressionPriority;

public class GenericConst<T> extends AbstractGenericExpression<T>{
    final private T value;

    public GenericConst(T value) {
        this.value = value;
    }

    @Override
    public T evaluate(T x, T y, T z) {
        return value;
    }

    @Override
    public T evaluate(T x) {
        return value;
    }

    @Override
    public int getPriority() {
        return ExpressionPriority.VALUE.priority;
    }

    @Override
    public boolean equals(Object that) {
        return that instanceof GenericConst<?> && ((GenericConst<?>) that).value.equals(this.value);
    }

    @Override
    public String toString() {
        return value.toString();
    }

    @Override
    public String toMiniString() {
        return this.toString();
    }
}
