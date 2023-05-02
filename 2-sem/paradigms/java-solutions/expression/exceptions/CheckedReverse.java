package expression.exceptions;

import expression.AllExpressions;
import expression.Reverse;

public class CheckedReverse extends Reverse {
    public CheckedReverse(AllExpressions value) {
        super(value);
    }

    public static int checkedCalculate(int x) throws OverflowEvaluateException {
        int result = 0;
        while (x != 0) {
            if ((Integer.MAX_VALUE / 10 < result && x > 0) || (Integer.MIN_VALUE / 10 > result && x < 0)) {
                throw new OverflowEvaluateException("Overflow");
            }

            result = result * 10 + x % 10;
            x /= 10;
        }
        return result;
    }

    @Override
    protected int calculate(int x) {
        try {
            return checkedCalculate(x);
        } catch (EvaluateException e) {
            throw new EvaluateException("Error in reverse \"" + value.toMiniString() + "\": " + e.getMessage());
        }
    }
}
