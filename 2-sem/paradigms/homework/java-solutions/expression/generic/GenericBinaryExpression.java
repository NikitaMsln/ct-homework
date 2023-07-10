package expression.generic;

import java.util.function.BinaryOperator;

public class GenericBinaryExpression<T> extends AbstractGenericExpression<T> {
    private final GenericExpression<T> first;
    private final GenericExpression<T> second;
    private final BinaryOperator<T> operator;
    private final BinaryOperatorDescription description;

    public GenericBinaryExpression(GenericExpression<T> first, GenericExpression<T> second, BinaryOperator<T> operator, BinaryOperatorDescription description) {
        this.first = first;
        this.second = second;
        this.operator = operator;
        this.description = description;
    }

    @Override
    public T evaluate(T x) {
        return operator.apply(first.evaluate(x), second.evaluate(x));
    }

    @Override
    public T evaluate(T x, T y, T z) {
        return operator.apply(first.evaluate(x, y, z), second.evaluate(x, y, z));
    }

    @Override
    public int getPriority() {
        return description.getPriority();
    }

    private boolean isSameOperator(Object that) {
        return that != null && that.getClass() == this.getClass();
    }

    @Override
    public boolean equals(Object that) {
        return (that instanceof GenericBinaryExpression<?>) &&
                isSameOperator(that) &&
                ((GenericBinaryExpression<?>) that).first.equals(this.first) &&
                ((GenericBinaryExpression<?>) that).second.equals(this.second);
    }

    @Override
    public String toString() {
        return "(" + first.toString() + " " + description.getName() + " " + second.toString() + ")";
    }

    @Override
    public String toMiniString() {
        String result = "";

        if (first.getPriority() < this.getPriority()) {
            result += "(" + first.toMiniString() + ")";
        } else {
            result += first.toMiniString();
        }

        result += " " + description.getName() +  " ";

        if (second.getPriority() < this.getPriority() ||
                (second.getPriority() == this.getPriority() &&
                        !(description.isAssociative() &&
                                (isSameOperator(second) || second instanceof GenericBinaryExpression<?> && !((GenericBinaryExpression<?>) second).description.isDependent())
                        )
                )
        ) {
            result += "(" + second.toMiniString() + ")";
        } else {
            result += second.toMiniString();
        }

        return result;
    }
}
