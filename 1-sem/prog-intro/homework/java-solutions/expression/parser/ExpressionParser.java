package expression.parser;

import expression.*;

public class ExpressionParser implements TripleParser {
    @Override
    public TripleExpression parse(String expression) {
        SymbolParser in = new SymbolParser(expression);
        TripleExpression result;
        try {
            result = sum(in);
        } catch (IncorrectExpressionException e) {
            System.out.println("Error: " + e.getMessage());
            result = null;
        }
        return result;
    }

    private AllExpressions sum(SymbolParser in) throws IncorrectExpressionException {

        AllExpressions result = product(in);

        while (in.hasNext() && (in.top() == '+' || in.top() == '-')) {
            if (in.next() == '+') {
                result = new Add(result, product(in));
            } else {
                result = new Subtract(result, product(in));
            }
        }

        return result;
    }

    private AllExpressions product(SymbolParser in) throws IncorrectExpressionException {

        AllExpressions result = element(in);

        while (in.hasNext() && (in.top() == '*' || in.top() == '/')) {
            if (in.next() == '*') {
                result = new Multiply(result, element(in));
            } else {
                result = new Divide(result, element(in));
            }
        }

        return result;
    }

    private AllExpressions element(SymbolParser in) throws IncorrectExpressionException {
        char c = in.next();
        if (c == '(') {
            AllExpressions result = sum(in);
            if (in.top() == ')') {
                in.next();
            }
            return result;
        } else if (c == '$') {
            return new Variable(parseVariable(in));
        } else if (c == '-') {
            if (Character.isDigit(in.top())) {
                return new Const(-1 * parseInt(in));
            } else {
                return new UnarMinus(element(in));
            }
        } else if (Character.isDigit(c)) {
            in.goBack();
            return new Const(parseInt(in));
        } else if (Character.isLetter(c)) {
            in.goBack();
            return new Variable(parseVariable(in));
        } else {
                throw new IncorrectExpressionException("Cannot parse expressions");
        }
    }

    private int parseInt(SymbolParser in) {
        int number = 0;
        while (in.hasNext() && Character.isDigit(in.top())) {
            number = number * 10 + in.next() - '0';
        }
        return number;
    }

    private String parseVariable(SymbolParser in) {
        StringBuilder result = new StringBuilder();
        while (in.hasNext() && Character.isLetter(in.top())) {
            result.append(in.next());
        }
        return result.toString();
    }
}