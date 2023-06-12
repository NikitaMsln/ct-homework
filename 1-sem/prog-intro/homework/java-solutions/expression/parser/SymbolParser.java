package expression.parser;

public class SymbolParser {
    private final String data;
    private int position;
    public SymbolParser(String data) {
        this.data = data;
        this.position = 0;
        skipWhitespace();
    }

    private void skipWhitespace() {
        while (position < data.length() && Character.isWhitespace(data.charAt(position))) {
            position++;
        }
    }

    public boolean hasNext() {
        skipWhitespace();
        return position < data.length();
    }

    public char next() throws IndexOutOfBoundsException {
        skipWhitespace();
        position++;
        return data.charAt(position - 1);
    }

    public char top() throws IndexOutOfBoundsException {
        skipWhitespace();
        return data.charAt(position);
    }

    public void goBack() throws IndexOutOfBoundsException {
        position--;
        while (Character.isWhitespace(data.charAt(position))) {
            position--;
        }
    }
}
