package expression;

public class Variable extends AbstractExpression {
    final private String name;

    public Variable(String name) {
        this.name = name;
    }

    @Override
    public int evaluate(int x, int y, int z) {
        return switch (name) {
            case "x" -> x;
            case "y" -> y;
            case "z" -> z;
            default -> throw new IllegalArgumentException("Unexpected name of variable: " + name);
        };
    }

    @Override
    public int evaluate(int x) {
        return x;
    }

    @Override
    public int getPriority() {
        return ExpressionPriority.VALUE.priority;
    }

    @Override
    public boolean equals(Object that) {
        return (that instanceof Variable) && ((Variable) that).name.equals(this.name);
    }

    @Override
    public String toString() {
        return name;
    }

    @Override
    public String toMiniString() {
        return name;
    }
}
