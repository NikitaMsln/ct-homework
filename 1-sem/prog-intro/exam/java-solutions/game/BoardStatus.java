package game;

public interface BoardStatus {
    Cell[][] getBoard();
    boolean isValid(Move move);
}
