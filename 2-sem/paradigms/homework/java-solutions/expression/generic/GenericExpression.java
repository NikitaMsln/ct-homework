package expression.generic;

import expression.PriorityExpression;
import expression.ToMiniString;

public interface GenericExpression<T> extends ToMiniString, PriorityExpression {
    T evaluate(T x);
    T evaluate(T x, T y, T z);
}
