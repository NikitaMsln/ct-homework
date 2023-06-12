package expression;

public class Divide extends AbstractBinaryExpression {

    public Divide(AllExpressions first, AllExpressions second) {
        super(first, second, "/",  20, false);
    }

    @Override
    public int evaluate(int variable) {
        return first.evaluate(variable) / second.evaluate(variable);
    }

    @Override
    public double evaluate(double variable) {
        return first.evaluate(variable) / second.evaluate(variable);
    }

    @Override
    public int evaluate(int x, int y, int z) {
        return first.evaluate(x, y, z) / second.evaluate(x, y, z);
    }
}
