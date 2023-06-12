package expression;

public abstract class AbstractBinaryExpression extends AbstractExpression {
    final protected AllExpressions first;
    final protected AllExpressions second;
    final boolean isCommutative;

    final protected String operator;

    protected AbstractBinaryExpression(AllExpressions first, AllExpressions second, String operator, int priority, boolean isCommutative) {
        super(priority);
        this.operator = operator;
        this.first = first;
        this.second = second;
        this.isCommutative = isCommutative;
    }

    @Override
    public String toString() {
        return "(" + first.toString() + " " + operator + " " + second.toString() + ")";
    }

    @Override
    public String toMiniString() {
        String result = "";

        if (first.getPriority() < this.getPriority()) {
            result += "(" + first.toMiniString() + ")";
        } else {
            result += first.toMiniString();
        }

        result += " " + operator +  " ";

        if (second.getPriority() < this.getPriority() || (second.getPriority() == this.getPriority() && !this.isCommutative)) {
            result += "(" + second.toMiniString() + ")";
        } else {
            result += second.toMiniString();
        }

        return result;
    }
}
