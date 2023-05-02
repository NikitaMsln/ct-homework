package expression;

public class GCD extends AbstractBinaryExpression {
    private final static boolean IS_DEPENDENT = true;
    private final static boolean IS_ASSOCIATIVE = true;

    public GCD(AllExpressions first, AllExpressions second) {
        super(first, second);
    }

    @Override
    public String getName() {
        return "gcd";
    }

    @Override
    public int getPriority() {
        return ExpressionPriority.GCDLCM.priority;
    }

    @Override
    protected int calculate(int x, int y) {
        return gcd(x, y);
    }

    @Override
    protected boolean isDependent() {
        return IS_DEPENDENT;
    }

    @Override
    protected boolean isAssociative() {
        return IS_ASSOCIATIVE;
    }

    public static int gcd(int x, int y) {
        long bigX = (x < 0)? -(long) x : (long) x;
        long bigY = (y < 0)? -(long) y : (long) y;
        while (bigX > 0 && bigY > 0) {
            if (bigX > bigY) {
                bigX = bigX % bigY;
            } else {
                bigY = bigY % bigX;
            }
        }
        return (int)(bigX + bigY);
    }
}
