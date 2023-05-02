package expression.exceptions;

import expression.Add;
import expression.AllExpressions;

public class CheckedAdd extends Add {
    public CheckedAdd(AllExpressions first, AllExpressions second) {
        super(first, second);
    }

    public static int checkedCalculate(int x, int y) throws OverflowEvaluateException {
        if ((x > 0 && Integer.MAX_VALUE - x < y) || (x < 0 && Integer.MIN_VALUE - x > y)) {
            throw new OverflowEvaluateException("Overflow");
        }
        return x + y;
    }

    @Override
    protected int calculate(int x, int y) {
        try {
            return checkedCalculate(x, y);
        } catch (EvaluateException e) {
            throw new EvaluateException("Error in sum \"" + first.toMiniString() + "\" and \"" + second.toMiniString() + "\": " + e.getMessage());
        }
    }
}
