package expression.exceptions;

import java.util.ArrayList;
import java.util.List;

public class TokenParser {
    private final List<String> tokens;
    private final List<Integer> tokenPositions;

    private int tokenIndex;

    public TokenParser(String data) {
        tokens = new ArrayList<>();
        tokenPositions = new ArrayList<>();

        boolean lastIsArgument = false;

        for (int i = 0; i < data.length();) {
            while (Character.isWhitespace(data.charAt(i))) {
                i++;
            }

            if (data.charAt(i) == '-') {
                i++;
                if (i < data.length() && Character.isDigit(data.charAt(i)) && !lastIsArgument) {
                    int tokenEnd = parseEndWord(data, i);
                    tokens.add(data.substring(i - 1, tokenEnd));
                    i = tokenEnd;
                } else {
                    tokens.add("-");
                }
            } else if (Character.isDigit(data.charAt(i)) || Character.isLetter(data.charAt(i))) {
                int tokenEnd = parseEndWord(data, i);
                tokens.add(data.substring(i, tokenEnd));
                i = tokenEnd;
            } else {
                tokens.add(Character.toString(data.charAt(i++)));
            }
            tokenPositions.add(i);
        }

        tokenIndex = 0;
    }

    private int parseEndWord(String data, int start) {
        while (start < data.length() && (
                        Character.isDigit(data.charAt(start)) ||
                        Character.isLetter(data.charAt(start)) ||
                        data.charAt(start) == '.' ||
                        data.charAt(start) == ','
        )) {
            start++;
        }
        return start;
    }

    public boolean hasNext() {
        return tokenIndex < tokens.size();
    }

    public String next() {
        return tokens.get(tokenIndex++);
    }

    public String top() {
        return tokens.get(tokenIndex);
    }

    public int getPosition() {
        if (tokenIndex == 0) {
            return 0;
        } else {
            return tokenPositions.get(tokenIndex - 1);
        }
    }

    public boolean test(final String expected) {
        if (top().equals(expected)) {
            next();
            return true;
        } else {
            return false;
        }
    }

    public boolean testChar(char expected) {
        if (tokens.get(tokenIndex).charAt(0) == expected) {
            if (tokens.get(tokenIndex).length() == 1) {
                next();
            } else {
                tokens.set(tokenIndex, tokens.get(tokenIndex).substring(1));
            }
            return true;
        } else {
            return false;
        }
    }
}
