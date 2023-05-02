package expression.exceptions;

import expression.AbstractUnaryExpression;
import expression.AllExpressions;

public class CheckedLog10 extends AbstractUnaryExpression {

    public CheckedLog10(AllExpressions value) {
        super(value);
    }

    public static int checkedCalculate(int x) throws IncorrectOperationArgument {
        if (x <= 0) {
            throw new IncorrectOperationArgument("Not positive argument of log");
        }

        int result;
        for (result = 0; x > 9; x /= 10, result++);

        return result;
    }

    @Override
    public String getName() {
        return "log10";
    }

    @Override
    protected int calculate(int x) {
        try {
            return checkedCalculate(x);
        } catch (EvaluateException e) {
            throw new EvaluateException("Error in log10 \"" + value.toMiniString() + "\": " + e.getMessage());
        }
    }
}
