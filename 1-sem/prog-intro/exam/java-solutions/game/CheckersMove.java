package game;

public class CheckersMove implements Move {
    final private int[] from;
    final private int[] to;

    public CheckersMove(int fromX, int fromY, int toX, int toY) {
        from = new int[] {fromX, fromY};
        to = new int[] {toX, toY};
    }

    @Override
    public int[] getFrom() {
        return from;
    }

    @Override
    public int[] getTo() {
        return to;
    }

    @Override
    public String toString() {
        return "From (" + from[0] + ", " + from[1] + ") to (" + to[0] + ", " + to[1] + ")";
    }
}
