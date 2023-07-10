package expression.generic;

public interface ExpressionGenerator<T> {
    GenericExpression<T> getAdd(GenericExpression<T> first, GenericExpression<T> second);
    GenericExpression<T> getDivide(GenericExpression<T> first, GenericExpression<T> second);
    GenericExpression<T> getMod(GenericExpression<T> first, GenericExpression<T> second);
    GenericExpression<T> getMultiply(GenericExpression<T> first, GenericExpression<T> second);
    GenericExpression<T> getSubtract(GenericExpression<T> first, GenericExpression<T> second);
    GenericExpression<T> getAbs(GenericExpression<T> value);
    GenericExpression<T> getNegate(GenericExpression<T> value);
    GenericExpression<T> getSquare(GenericExpression<T> value);
    GenericExpression<T> getConst(String value);
    GenericExpression<T> getVariable(String value);
    T valueOf(int value);
}
