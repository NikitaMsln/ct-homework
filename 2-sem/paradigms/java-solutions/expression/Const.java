package expression;

public class Const extends AbstractExpression {
    final private int value;

    public Const(int value) {
        this.value = value;
    }

    @Override
    public int evaluate(int x, int y, int z) {
        return value;
    }

    @Override
    public int evaluate(int x) {
        return value;
    }

    @Override
    public int getPriority() {
        return ExpressionPriority.VALUE.priority;
    }

    @Override
    public boolean equals(Object that) {
        return that instanceof Const && ((Const) that).value == this.value;
    }

    @Override
    public String toString() {
        return String.valueOf(value);
    }

    @Override
    public String toMiniString() {
        return this.toString();
    }
}
