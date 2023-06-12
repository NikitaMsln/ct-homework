package expression;

public class Const extends AbstractExpression {
    final private int iValue;
    final private double dValue;

    final private boolean isInteger;

    public Const(int value) {
        super(100);
        this.iValue = value;
        this.dValue = value;
        this.isInteger = true;
    }

    public Const(double value) {
        super(100);
        this.dValue = value;
        this.iValue = (int) value;
        this.isInteger = false;
    }

    @Override
    public int evaluate(int variable) {
        return iValue;
    }

    @Override
    public double evaluate(double variable) {
        return dValue;
    }

    @Override
    public int evaluate(int x, int y, int z) {
        return iValue;
    }

    @Override
    public String toString() {
        if (isInteger) {
            return String.valueOf(iValue);
        } else {
            return String.valueOf(dValue);
        }
    }

    @Override
    public String toMiniString() {
        return this.toString();
    }
}
