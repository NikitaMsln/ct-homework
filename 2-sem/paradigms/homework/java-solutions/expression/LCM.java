package expression;

public class LCM extends AbstractBinaryExpression {
    private final static boolean IS_DEPENDENT = true;
    private final static boolean IS_ASSOCIATIVE = true;

    public LCM(AllExpressions first, AllExpressions second) {
        super(first, second);
    }

    @Override
    public String getName() {
        return "lcm";
    }

    @Override
    public int getPriority() {
        return ExpressionPriority.GCDLCM.priority;
    }

    @Override
    protected int calculate(int x, int y) {
        return lcm(x, y);
    }

    @Override
    protected boolean isDependent() {
        return IS_DEPENDENT;
    }

    @Override
    protected boolean isAssociative() {
        return IS_ASSOCIATIVE;
    }

    public static int lcm(int x, int y) {
        if (x == 0 && y == 0) {
            return 0;
        } else {
            return (int)((long)x * (long) y / (long) GCD.gcd(x, y));
        }
    }
}
