package game;

public interface Board {
    Cell getTurn();
    Result makeMove(Move move);
    boolean isValid(Move move);
    Cell getCell(int r, int c);
}
