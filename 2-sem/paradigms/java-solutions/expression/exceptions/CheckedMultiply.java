package expression.exceptions;

import expression.AllExpressions;
import expression.Multiply;

public class CheckedMultiply extends Multiply {

    public CheckedMultiply(AllExpressions first, AllExpressions second) {
        super(first, second);
    }

    public static int checkedCalculate(int x, int y) {

        if (x == 0 || y == 0) {
            return 0;
        }

        int max = (x < 0 && y < 0 || x > 0 && y > 0) ? Integer.MAX_VALUE : Integer.MIN_VALUE;

        if (x == -1) {
            if (y == Integer.MIN_VALUE) {
                throw new OverflowEvaluateException("Overflow");
            } else {
                return x * y;
            }
        }

        if (y > 0 && y > max / x || y < 0 && y < max / x) {
            throw new OverflowEvaluateException("Overflow");
        }

        return x * y;
    }

    @Override
    protected int calculate(int x, int y) {
        try {
            return checkedCalculate(x, y);
        } catch (EvaluateException e) {
            throw new EvaluateException("Error in multiply \"" + first.toMiniString() + "\" and \"" + second.toMiniString() + "\": " + e.getMessage());
        }
    }
}
