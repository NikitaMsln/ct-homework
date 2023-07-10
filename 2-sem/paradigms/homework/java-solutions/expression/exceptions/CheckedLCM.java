package expression.exceptions;

import expression.*;

public class CheckedLCM extends LCM {

    public CheckedLCM(AllExpressions first, AllExpressions second) {
        super(first, second);
    }

    public static int checkedCalculate(int x, int y) throws OverflowEvaluateException {
        if (x == y) {
            return (x < 0)? -x : x;
        }

        x = x / GCD.gcd(x, y);

        return CheckedMultiply.checkedCalculate(x, y);
    }

    @Override
    protected int calculate(int x, int y) {
        try {
            return checkedCalculate(x, y);
        } catch (EvaluateException e) {
            throw new EvaluateException("Error in lcm \"" + first.toMiniString() + "\" and \"" + second.toMiniString() + "\": " + e.getMessage());
        }
    }
}
