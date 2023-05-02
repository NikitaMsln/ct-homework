package expression;

public abstract class AbstractBinaryExpression extends AbstractExpression implements OperatorExpression {
    protected AllExpressions first;
    protected AllExpressions second;

    protected AbstractBinaryExpression(AllExpressions first, AllExpressions second) {
        this.first = first;
        this.second = second;
    }

    protected abstract int calculate(int x, int y);
    protected abstract boolean isDependent();
    protected abstract boolean isAssociative();

    @Override
    public int evaluate(int x, int y, int z) {
        return calculate(first.evaluate(x, y, z), second.evaluate(x, y ,z));
    }

    @Override
    public int evaluate(int x) {
        return calculate(first.evaluate(x), second.evaluate(x));
    }

    private boolean isSameOperator(Object that) {
        return that != null && that.getClass() == this.getClass();
    }

    @Override
    public boolean equals(Object that) {
        return (that instanceof AbstractBinaryExpression) &&
                ((AbstractBinaryExpression) that).first.equals(this.first) &&
                ((AbstractBinaryExpression) that).second.equals(this.second) &&
                isSameOperator(that);
    }

    @Override
    public String toString() {
        return "(" + first.toString() + " " + this.getName() + " " + second.toString() + ")";
    }

    @Override
    public String toMiniString() {
        String result = "";

        if (first.getPriority() < this.getPriority()) {
            result += "(" + first.toMiniString() + ")";
        } else {
            result += first.toMiniString();
        }

        result += " " + this.getName() +  " ";

        if (second.getPriority() < this.getPriority() || (second.getPriority() == this.getPriority() && !(this.isAssociative() && (isSameOperator(second) || second instanceof AbstractBinaryExpression && !((AbstractBinaryExpression) second).isDependent())))) {
            result += "(" + second.toMiniString() + ")";
        } else {
            result += second.toMiniString();
        }

        return result;
    }
}
