package expression.generic.generators;

import expression.exceptions.*;
import expression.generic.GenericConst;
import expression.generic.GenericExpression;
import expression.generic.GenericVariable;

public class CheckedIntegerGenerator extends AbstractGenerator<Integer> {
    @Override
    protected Integer add(Integer x, Integer y) {
        try {
            return CheckedAdd.checkedCalculate(x, y);
        } catch (EvaluateException e) {
            throw new EvaluateException("Error in checked integer sum \"" + x + "\" and \"" + y + "\": " + e.getMessage());
        }
    }

    @Override
    protected Integer divide(Integer x, Integer y) {
        try {
            return CheckedDivide.checkedCalculate(x, y);
        } catch (EvaluateException e) {
            throw new EvaluateException("Error in checked integer multiply \"" + x + "\" and \"" + y + "\": " + e.getMessage());
        }
    }

    @Override
    protected Integer module(Integer x, Integer y) {
        if (y == 0) {
            throw new EvaluateException("Error in checked integer mod \"" + x + "\" and \"" + y + "\": division by zero");
        }
        return x % y;
    }

    @Override
    protected Integer multiply(Integer x, Integer y) {
        try {
            return CheckedMultiply.checkedCalculate(x, y);
        } catch (EvaluateException e) {
            throw new EvaluateException("Error in checked integer multiply \"" + x + "\" and \"" + y + "\": " + e.getMessage());
        }
    }

    @Override
    protected Integer subtract(Integer x, Integer y) {
        try {
            return CheckedSubtract.checkedCalculate(x, y);
        } catch (EvaluateException e) {
            throw new EvaluateException("Error in checked integer subtract \"" + x + "\" and \"" + y + "\": " + e.getMessage());
        }
    }

    @Override
    protected Integer abs(Integer x) {
        try {
            return (x < 0) ? CheckedNegate.checkedCalculate(x) : x;
        } catch (EvaluateException e) {
            throw new EvaluateException("Error in checked integer abs \"" + x + "\": " + e.getMessage());
        }
    }

    @Override
    protected Integer negate(Integer x) {
        try {
            return CheckedNegate.checkedCalculate(x);
        } catch (EvaluateException e) {
            throw new EvaluateException("Error in checked integer negate \"" + x + "\": " + e.getMessage());
        }
    }

    @Override
    protected Integer square(Integer x) {
        try {
            return CheckedMultiply.checkedCalculate(x, x);
        } catch (EvaluateException e) {
            throw new EvaluateException("Error in checked integer square \"" + x + "\": " + e.getMessage());
        }
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