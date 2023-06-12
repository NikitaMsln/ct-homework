package game;

public interface Player {
    Move makeMove(BoardStatus board) throws IncorrectPlayersMove;
    String getName();
}
