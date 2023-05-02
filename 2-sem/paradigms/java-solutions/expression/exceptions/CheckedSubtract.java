package expression.exceptions;

import expression.AllExpressions;
import expression.Subtract;

public class CheckedSubtract extends Subtract {
    public CheckedSubtract(AllExpressions first, AllExpressions second) {
        super(first, second);
    }

    public static int checkedCalculate(int x, int y) {
        if ((y < 0 && Integer.MAX_VALUE + y < x) || (y > 0 && Integer.MIN_VALUE + y > x)) {
            throw new OverflowEvaluateException("Overflow");
        }
        return x - y;
    }

    @Override
    protected int calculate(int x, int y) {
        try {
            return checkedCalculate(x, y);
        } catch (EvaluateException e) {
            throw new EvaluateException("Error in subtract \"" + first.toMiniString() + "\" and \"" + second.toMiniString() + "\": " + e.getMessage());
        }
    }
}
