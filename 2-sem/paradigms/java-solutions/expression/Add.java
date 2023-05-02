package expression;

public class Add extends AbstractBinaryExpression {
    private final static boolean IS_DEPENDENT = false;
    private final static boolean IS_ASSOCIATIVE = true;

    public Add(AllExpressions first, AllExpressions second) {
        super(first, second);
    }

    @Override
    public String getName() {
        return "+";
    }

    @Override
    public int getPriority() {
        return ExpressionPriority.ADDITIVE.priority;
    }

    @Override
    protected int calculate(int x, int y) {
        return x + y;
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
