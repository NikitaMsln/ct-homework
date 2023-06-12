package game;

public class CheckersStatus implements BoardStatus{
    final private Checkers board;

    public CheckersStatus(Checkers board) {
        this.board = board;
    }

    @Override
    public Cell[][] getBoard() {
        Cell[][] boardCopy = new Cell[Checkers.COLS][Checkers.ROWS];
        for (int i = 0; i < Checkers.COLS; i++) {
            for (int j = 0; j < Checkers.ROWS; j++) {
                boardCopy[i][j] = board.getCell(i, j);
            }
        }
        return boardCopy;
    }

    @Override
    public boolean isValid(Move move) {
        return board.isValid(move);
    }
}
