package expression;

public class Divide extends AbstractBinaryExpression {
    private final static boolean IS_DEPENDENT = true;
    private final static boolean IS_ASSOCIATIVE = false;

    public Divide(AllExpressions first, AllExpressions second) {
        super(first, second);
    }

    @Override
    public String getName() {
        return "/";
    }

    @Override
    public int getPriority() {
        return ExpressionPriority.MULTIPLICATIVE.priority;
    }

    @Override
    protected int calculate(int x, int y) {
        return x / y;
    }

    @Override
    protected boolean isDependent() {
        return IS_DEPENDENT;
    }

    @Override
    protected boolean isAssociative() {
        return IS_ASSOCIATIVE;
    }
}
