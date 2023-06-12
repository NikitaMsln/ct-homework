package expression;

public interface Expression {
    String toMiniString();
    int evaluate(int variable);
}
