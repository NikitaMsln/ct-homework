package expression;

public abstract class AbstractUnaryExpression extends AbstractExpression implements OperatorExpression {
    protected AllExpressions value;

    public AbstractUnaryExpression(AllExpressions value) {
        this.value = value;
    }

    protected abstract int calculate(int x);

    @Override
    public int evaluate(int x, int y, int z) {
        return calculate(value.evaluate(x, y, z));
    }

    @Override
    public int evaluate(int x) {
        return calculate(value.evaluate(x));
    }

    @Override
    public boolean equals(Object that) {
        return (that instanceof AbstractUnaryExpression) &&
                ((AbstractUnaryExpression) that).value.equals(this.value) &&
                that.getClass() == this.getClass();
    }

    @Override
    public int getPriority() {
        return ExpressionPriority.UNARY.priority;
    }

    @Override
    public String toString() {
        return this.getName() + "(" + value.toString() + ")";
    }

    @Override
    public String toMiniString() {
        if (value.getPriority() >= this.getPriority()) {
            return this.getName() + " " + value.toMiniString();
        } else {
            return this.getName() + "(" + value.toMiniString() + ")";
        }
    }
}
