package expression;

public class Multiply extends AbstractBinaryExpression {

    public Multiply(AllExpressions first, AllExpressions second) {
        super(first, second, "*",  20, true);
    }

    @Override
    public int evaluate(int variable) {
        return first.evaluate(variable) * second.evaluate(variable);
    }

    @Override
    public double evaluate(double variable) {
        return first.evaluate(variable) * second.evaluate(variable);
    }

    @Override
    public int evaluate(int x, int y, int z) {
        return first.evaluate(x, y, z) * second.evaluate(x, y, z);
    }
}
