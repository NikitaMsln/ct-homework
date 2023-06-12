package game;

public class MovesManager implements Position {

    private final Board board;

    public MovesManager(Board board) {
        this.board = board;
    }

    @Override
    public boolean isValid(Move move) {
        return board.isValid(move);
    }

    @Override
    public Cell getCell(int r, int c) {
        return board.getCell(r, c);
    }

    @Override
    public String getBoard() { return board.toString(); }

}
