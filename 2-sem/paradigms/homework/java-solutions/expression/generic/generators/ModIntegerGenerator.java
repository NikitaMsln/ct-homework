package expression.generic.generators;

import expression.exceptions.DivisionByZeroEvaluateException;
import expression.generic.GenericConst;
import expression.generic.GenericExpression;
import expression.generic.GenericVariable;

public class ModIntegerGenerator extends AbstractGenerator<Integer> {
    public static final int MODE = 10079;

    public static int module(int x) {
        return ((x % MODE) + MODE) % MODE;
    }

    @Override
    protected Integer add(Integer x, Integer y) {
        return module(x + y);
    }

    @Override
    protected Integer divide(Integer x, Integer y) {
        if (y == 0) {
            throw new DivisionByZeroEvaluateException("Error in mod integer divide \"" + x + "\" and \"" + y + "\": division by zero");
        }
        y = module(y);
        int d = 1;
        for (int i = MODE - 2; i > 0; i = i >> 1, y = y * y % MODE) {
            if (i % 2 == 1) {
                d = d * y % MODE;
            }
        }
        return d * x % MODE;
    }

    @Override
    protected Integer module(Integer x, Integer y) {
        if (y == 0) {
            throw new DivisionByZeroEvaluateException("Error in mod integer mod \"" + x + "\" and \"" + y + "\": division by zero");
        }
        return module(x % y);
    }

    @Override
    protected Integer multiply(Integer x, Integer y) {
        return module(x * y);
    }

    @Override
    protected Integer subtract(Integer x, Integer y) {
        return  module(x - y);
    }

    @Override
    protected Integer abs(Integer x) {
        return module((x < 0)? -x : x);
    }

    @Override
    protected Integer negate(Integer x) {
        return module(-x);
    }

    @Override
    protected Integer square(Integer x) {
        return module(x * x);
    }

    @Override
    public GenericExpression<Integer> getConst(String value) {
        return new GenericConst<>(module(Integer.parseInt(value)));
    }

    @Override
    public GenericExpression<Integer> getVariable(String value) {
        return new GenericVariable<>(value, ModIntegerGenerator::module);
    }

    @Override
    public Integer valueOf(int value) {
        return module(value);
    }
}
