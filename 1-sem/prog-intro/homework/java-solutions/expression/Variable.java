package expression;

public class Variable extends AbstractExpression {
    final private String name;

    public Variable(String name) {
        super(100);
        this.name = name;
    }

    @Override
    public int evaluate(int variable) {
        return variable;
    }

    @Override
    public double evaluate(double variable) {
        return variable;
    }

    @Override
    public int evaluate(int x, int y, int z) {
        switch (name) {
            case "x" -> { return x; }
            case "y" -> { return y; }
            case "z" -> { return z; }
            default -> { throw new IllegalArgumentException("Unexpected name of variable: " + name); }
        }
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
