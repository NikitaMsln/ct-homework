package expression.generic;

import expression.exceptions.*;

public class ExpressionParser<T> {
    private final ExpressionGenerator<T> generator;

    public ExpressionParser(ExpressionGenerator<T> generator) {
        this.generator = generator;
    }
    public GenericExpression<T> parse(final String expression) throws ParsingException {
        final TokenParser in = new TokenParser(expression);

        final GenericExpression<T> result = sum(in);
        if (in.hasNext()) {
            throw new IncorrectOperatorException("Expected binary operator but found '" + in.top() + "' at char " + in.getPosition());
        }
        return result;
    }

    private GenericExpression<T> sum(final TokenParser in) throws ParsingException {
        GenericExpression<T> result = product(in);

        while (in.hasNext()) {
            if (in.test("+")) {
                result = generator.getAdd(result, product(in));
            } else if (in.testChar('-')) {
                result = generator.getSubtract(result, product(in));
            } else {
                break;
            }
        }

        return result;
    }

    private GenericExpression<T> product(final TokenParser in) throws ParsingException {

        GenericExpression<T> result = element(in);

        while (in.hasNext()) {
            if (in.test("mod")) {
                result = generator.getMod(result, element(in));
            } else if (in.test("*")) {
                result = generator.getMultiply(result, element(in));
            } else if (in.test("/")) {
                result = generator.getDivide(result, element(in));
            } else {
                break;
            }
        }

        return result;
    }

    private GenericExpression<T> element(final TokenParser in) throws ParsingException {
        if (!in.hasNext()) {
            throw new ParsingIncorrectArgument("Argument of operator not found at char " + in.getPosition());
        }
        String token = in.next();
        if (token.equals("(")) {
            final GenericExpression<T> result = sum(in);
            if (in.hasNext()) {
                token = in.next();
                if (!token.equals(")")) {
                    throw new IncorrectBracketSequence("Expected ')' but found '" + token + "' at char " + in.getPosition());
                }
            } else {
                throw new IncorrectBracketSequence("Incorrect bracket sequence");
            }
            return result;
        } else if (token.equals("-")) {
            return generator.getNegate(element(in));
        } else if (Character.isDigit(token.charAt(0)) || token.charAt(0) == '-') {
            try {
                return generator.getConst(token);
            } catch (final NumberFormatException e) {
                throw new ParsingIncorrectArgument("Incorrect constant format '" + token + "' at char " + in.getPosition());
            }
        } else if (Character.isLetter(token.charAt(0))) {
            return switch (token) {
                case "abs" -> generator.getAbs(element(in));
                case "square" -> generator.getSquare(element(in));
                case "x", "y", "z" -> generator.getVariable(token);
                default -> throw new ParsingIncorrectArgument("Name of variable is not supported '" + token + "' at char " + in.getPosition());
            };
        } else {
            throw new IncorrectOperatorException("Expected unary operator but found '" + token + "' at char " + in.getPosition());
        }
    }
}
