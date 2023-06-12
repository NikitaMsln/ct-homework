package expression;

public abstract class AbstractExpression implements AllExpressions {
    private final int priority;

    protected AbstractExpression(int priority) {
        this.priority = priority;
    }

    public int getPriority() {
        return priority;
    }

    @Override
    public boolean equals(Object that) {
        if (that == null) {
            return false;
        }
        return this.toString().equals(that.toString());
    }

    @Override
    public int hashCode() {
        return this.toString().hashCode();
    }
}
