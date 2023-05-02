package expression.exceptions;

import expression.AllExpressions;
import expression.GCD;

public class CheckedGCD extends GCD {

    public CheckedGCD(AllExpressions first, AllExpressions second) {
        super(first, second);
    }

    public static int checkedCalculate(int x, int y) {
        return GCD.gcd(x, y);
    }

    @Override
    protected int calculate(int x, int y) {
        try {
            return checkedCalculate(x, y);
        } catch (EvaluateException e) {
            throw new EvaluateException("Error in gcd \"" + first.toMiniString() + "\" and \"" + second.toMiniString() + "\": " + e.getMessage());
        }
    }
}
