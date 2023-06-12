package expression;

public class Subtract extends AbstractBinaryExpression {

    public Subtract(AllExpressions first, AllExpressions second) {
        super(first, second, "-",  10, false);
    }

    @Override
    public int evaluate(int variable) {
        return first.evaluate(variable) - second.evaluate(variable);
    }

    @Override
    public double evaluate(double variable) {
        return first.evaluate(variable) - second.evaluate(variable);
    }

    @Override
    public int evaluate(int x, int y, int z) {
        return first.evaluate(x, y, z) - second.evaluate(x, y, z);
    }
}
