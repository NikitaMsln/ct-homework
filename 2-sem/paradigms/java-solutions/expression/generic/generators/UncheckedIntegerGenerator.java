package expression.generic.generators;

import expression.generic.GenericConst;
import expression.generic.GenericExpression;
import expression.generic.GenericVariable;

public class UncheckedIntegerGenerator extends AbstractGenerator<Integer> {
    @Override
    protected Integer add(Integer x, Integer y) {
        return x + y;
    }

    @Override
    protected Integer divide(Integer x, Integer y) {
        return x / y;
    }

    @Override
    protected Integer module(Integer x, Integer y) {
        return x % y;
    }

    @Override
    protected Integer multiply(Integer x, Integer y) {
        return x * y;
    }

    @Override
    protected Integer subtract(Integer x, Integer y) {
        return  x - y;
    }

    @Override
    protected Integer abs(Integer x) {
        return (x < 0)? -x : x;
    }

    @Override
    protected Integer negate(Integer x) {
        return -x;
    }

    @Override
    protected Integer square(Integer x) {
        return x * x;
    }

    @Override
    public GenericExpression<Integer> getConst(String value) {
        return new GenericConst<>(Integer.valueOf(value));
    }

    @Override
    public GenericExpression<Integer> getVariable(String value) {
        return new GenericVariable<>(value, x -> x);
    }

    @Override
    public Integer valueOf(int value) {
        return value;
    }
}
