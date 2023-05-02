package expression.generic.generators;

import expression.generic.*;

public class DoubleGenerator extends AbstractGenerator<Double> {
    @Override
    protected Double add(Double x, Double y) {
        return x + y;
    }

    @Override
    protected Double divide(Double x, Double y) {
        return x / y;
    }

    @Override
    protected Double module(Double x, Double y) {
        return x % y;
    }

    @Override
    protected Double multiply(Double x, Double y) {
        return x * y;
    }

    @Override
    protected Double subtract(Double x, Double y) {
        return x - y;
    }

    @Override
    protected Double abs(Double x) {
        return Math.sqrt(x * x);
    }

    @Override
    protected Double negate(Double x) {
        return -x;
    }

    @Override
    protected Double square(Double x) {
        return x * x;
    }

    @Override
    public GenericExpression<Double> getConst(String value) {
        return new GenericConst<>(Double.valueOf(value));
    }

    @Override
    public GenericExpression<Double> getVariable(String value) {
        return new GenericVariable<>(value, x -> x);
    }

    @Override
    public Double valueOf(int value) {
        return (double)value;
    }
}
