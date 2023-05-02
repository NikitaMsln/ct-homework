package expression.exceptions;

import expression.AllExpressions;
import expression.Divide;

public class CheckedDivide extends Divide {
    public CheckedDivide(AllExpressions first, AllExpressions second) {
        super(first, second);
    }

    public static int checkedCalculate(int x, int y) throws EvaluateException {
        if (y == 0) {
            throw new DivisionByZeroEvaluateException("Division by zero");
        }
        if (y == -1 && x == Integer.MIN_VALUE) {
            throw new OverflowEvaluateException("Overflow");
        }
        return x / y;
    }

    @Override
    protected int calculate(int x, int y) {
        try {
            return checkedCalculate(x, y);
        } catch (EvaluateException e) {
            throw new EvaluateException("Error in divide \"" + first.toMiniString() + "\" and \"" + second.toMiniString() + "\": " + e.getMessage());
        }
    }
}
