package expression.generic.generators;

import expression.generic.*;

public class ShortGenerator extends AbstractGenerator<Short> {
    @Override
    protected Short add(Short x, Short y) {
        return (short)(x + y);
    }

    @Override
    protected Short divide(Short x, Short y) {
        return (short)(x / y);
    }

    @Override
    protected Short module(Short x, Short y) {
        return (short)(x % y);
    }

    @Override
    protected Short multiply(Short x, Short y) {
        return (short)(x * y);
    }

    @Override
    protected Short subtract(Short x, Short y) {
        return (short)(x - y);
    }

    @Override
    protected Short abs(Short x) {
        return (short)( (x < 0)? -x : x);
    }

    @Override
    protected Short negate(Short x) {
        return (short)(-x);
    }

    @Override
    protected Short square(Short x) {
        return (short)(x * x);
    }

    @Override
    public GenericExpression<Short> getConst(String value) {
        return new GenericConst<>(Short.valueOf(value));
    }

    @Override
    public GenericExpression<Short> getVariable(String value) {
        return new GenericVariable<>(value, x -> x);
    }

    @Override
    public Short valueOf(int value) {
        return (short)value;
    }
}
