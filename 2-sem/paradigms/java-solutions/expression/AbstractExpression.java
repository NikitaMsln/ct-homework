package expression;

public abstract class AbstractExpression implements AllExpressions {
    @Override
    public int hashCode() {
        return this.toString().hashCode();
    }
}
