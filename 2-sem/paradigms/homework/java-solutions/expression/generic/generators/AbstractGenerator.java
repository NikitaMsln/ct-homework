package expression.generic.generators;

import expression.generic.*;

public abstract class AbstractGenerator<T> implements ExpressionGenerator<T> {
    protected abstract T add(T x, T y);
    protected abstract T divide(T x, T y);
    protected abstract T module(T x, T y);
    protected abstract T multiply(T x, T y);
    protected abstract T subtract(T x, T y);
    protected abstract T abs(T x);
    protected abstract T negate(T x);
    protected abstract T square(T x);

    @Override
    public GenericExpression<T> getAdd(final GenericExpression<T> first, final GenericExpression<T> second) {
        return new GenericBinaryExpression<>(first, second, this::add, BinaryOperatorDescription.ADD);
    }

    @Override
    public GenericExpression<T> getDivide(final GenericExpression<T> first, final GenericExpression<T> second) {
        return new GenericBinaryExpression<>(first, second, this::divide, BinaryOperatorDescription.DIVIDE);
    }

    @Override
    public GenericExpression<T> getMod(final GenericExpression<T> first, final GenericExpression<T> second) {
        return new GenericBinaryExpression<>(first, second, this::module, BinaryOperatorDescription.MOD);
    }

    @Override
    public GenericExpression<T> getMultiply(final GenericExpression<T> first, final GenericExpression<T> second) {
        return new GenericBinaryExpression<>(first, second, this::multiply, BinaryOperatorDescription.MULTIPLY);
    }

    @Override
    public GenericExpression<T> getSubtract(final GenericExpression<T> first, final GenericExpression<T> second) {
        return new GenericBinaryExpression<>(first, second, this::subtract, BinaryOperatorDescription.SUBTRACT);
    }

    @Override
    public GenericExpression<T> getAbs(final GenericExpression<T> value) {
        return new GenericUnaryExpression<>(value, this::abs, "abs");
    }

    @Override
    public GenericExpression<T> getNegate(final GenericExpression<T> value) {
        return new GenericUnaryExpression<>(value, this::negate, "-");
    }

    @Override
    public GenericExpression<T> getSquare(final GenericExpression<T> value) {
        return new GenericUnaryExpression<>(value, this::square, "square");
    }
}
