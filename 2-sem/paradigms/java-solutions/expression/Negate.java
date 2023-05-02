package expression;

public class Negate extends AbstractUnaryExpression {
    public Negate(AllExpressions value) {
        super(value);
    }

    @Override
    public String getName() {
        return "-";
    }

    @Override
    protected int calculate(int x) {
        return -x;
    }
}
