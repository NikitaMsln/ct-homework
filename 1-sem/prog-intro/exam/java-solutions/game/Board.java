package game;

public interface Board {
    Cell getCell(int x, int y);
    boolean isValid(Move move);
    MoveResult makeMove(Move move);
    BoardStatus getStatus();
    int getPlayersCount();
}
