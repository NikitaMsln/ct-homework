package expression;

public class UnarMinus extends AbstractExpression {
    AllExpressions value;

    public UnarMinus(AllExpressions value) {
        super(20);
        this.value = value;
    }

    @Override
    public int evaluate(int variable) {
        return - value.evaluate(variable);
    }

    @Override
    public double evaluate(double variable) {
        return - value.evaluate(variable);
    }

    @Override
    public int evaluate(int x, int y, int z) {
        return - value.evaluate(x, y, z);
    }

    @Override
    public String toString() {
        return "-(" + value.toString() + ")";
    }

    @Override
    public String toMiniString() {
        return "-(" + value.toMiniString() + ")";
    }
}
