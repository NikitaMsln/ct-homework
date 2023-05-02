package expression.generic;

public abstract class AbstractGenericExpression<T> implements GenericExpression<T> {
    @Override
    public int hashCode() {
        return this.toString().hashCode();
    }
}
