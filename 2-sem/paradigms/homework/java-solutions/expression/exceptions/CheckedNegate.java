package expression.exceptions;

import expression.AllExpressions;
import expression.Negate;

public class CheckedNegate extends Negate {

    public CheckedNegate(AllExpressions value) {
        super(value);
    }

    public static int checkedCalculate(int x) throws OverflowEvaluateException {
        if (x < -Integer.MAX_VALUE) {
            throw new OverflowEvaluateException("Overflow");
        }
        return -x;
    }

    @Override
    protected int calculate(int x) {
        try {
            return checkedCalculate(x);
        } catch (EvaluateException e) {
            throw new EvaluateException("Error in negate \"" + value.toMiniString() + "\": " + e.getMessage());
        }
    }
}
