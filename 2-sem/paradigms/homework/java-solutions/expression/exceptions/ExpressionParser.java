package expression.exceptions;

import expression.*;

public class ExpressionParser implements TripleParser {
    @Override
    public TripleExpression parse(String expression) throws ParsingException {
        TokenParser in = new TokenParser(expression);

        AllExpressions result = gcdAndlcm(in);
        if (in.hasNext()) {
            throw new IncorrectOperatorException("Expected binary operator but found '" + in.top() + "' at char " + in.getPosition());
        }
        return result;
    }

    private AllExpressions gcdAndlcm(TokenParser in) throws ParsingException {
        AllExpressions result = sum(in);
        while (in.hasNext()) {
            if (in.test("gcd")) {
                result = new CheckedGCD(result, sum(in));
            } else if (in.test("lcm")) {
                result = new CheckedLCM(result, sum(in));
            } else {
                break;
            }
        }

        return result;
    }

    private AllExpressions sum(TokenParser in) throws ParsingException {
        AllExpressions result = product(in);

        while (in.hasNext()) {
            if (in.test("+")) {
                result = new CheckedAdd(result, product(in));
            } else if (in.testChar('-')) {
                result = new CheckedSubtract(result, product(in));
            } else {
                break;
            }
        }

        return result;
    }

    private AllExpressions product(TokenParser in) throws ParsingException {

        AllExpressions result = element(in);

        while (in.hasNext()) {
            if (in.test("*")) {
                result = new CheckedMultiply(result, element(in));
            } else if (in.test("/")) {
                result = new CheckedDivide(result, element(in));
            } else {
                break;
            }
        }

        return result;
    }

    private AllExpressions element(TokenParser in) throws ParsingException {
        if (!in.hasNext()) {
            throw new ParsingIncorrectArgument("Argument of operator not found at char " + in.getPosition());
        }
        String token = in.next();
        if (token.equals("(")) {
            AllExpressions result = gcdAndlcm(in);
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
            return new CheckedNegate(element(in));
        } else if (Character.isDigit(token.charAt(0)) || token.charAt(0) == '-') {
            return getConst(token, in.getPosition());
        } else if (Character.isLetter(token.charAt(0))) {
            return switch (token) {
                case "reverse" -> new CheckedReverse(element(in));
                case "pow10" -> new CheckedPow10(element(in));
                case "log10" -> new CheckedLog10(element(in));
                case "x", "y", "z" -> new Variable(token);
                default -> throw new ParsingIncorrectArgument("Name of variable is not supported '" + token + "' at char " + in.getPosition());
            };
        } else {
            throw new IncorrectOperatorException("Expected unary operator but found '" + token + "' at char " + in.getPosition());
        }
    }

    private Const getConst(String value, int position) throws ParsingException {
        try {
            return new Const(Integer.parseInt(value));
        } catch (NumberFormatException e) {
            throw new ParsingIncorrectArgument("Incorrect constant format '" + value + "' at char " + position);
        }
    }
}
