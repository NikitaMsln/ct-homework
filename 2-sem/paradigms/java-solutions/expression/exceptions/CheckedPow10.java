package expression.exceptions;

import expression.AbstractUnaryExpression;
import expression.AllExpressions;

public class CheckedPow10 extends AbstractUnaryExpression {
    public CheckedPow10(AllExpressions value) {
        super(value);
    }

    public static int checkedCalculate(int x) throws EvaluateException {
        if (x < 0) {
            throw new IncorrectOperationArgument("Negative argument of integer pow");
        }

        int result = 1;
        for (int i = 0; i < x; i++) {
            if (Integer.MAX_VALUE / 10 < result) {
                throw new OverflowEvaluateException("Overflow");
            }
            result *= 10;
        }

        return result;
    }

    @Override
    public String getName() {
        return "pow10";
    }

    @Override
    protected int calculate(int x) {
        try {
            return checkedCalculate(x);
        } catch (EvaluateException e) {
            throw new EvaluateException("Error in pow10 \"" + value.toMiniString() + "\": " + e.getMessage());
        }
    }
}
