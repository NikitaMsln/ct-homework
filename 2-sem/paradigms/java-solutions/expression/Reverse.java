package expression;

public class Reverse extends AbstractUnaryExpression {
    public Reverse(AllExpressions value) {
        super(value);
    }

    public static int reverse(int x) {

        int result = 0;
        while (x != 0) {
            result = result * 10 + x % 10;
            x /= 10;
        }
        return result;
    }

    @Override
    public String getName() {
        return "reverse";
    }

    @Override
    protected int calculate(int x) {
        return reverse(x);
    }
}
