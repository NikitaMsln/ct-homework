package game;

public class Checkers implements Board {
    // :NOTE: - missorted modifiers
    final static public int PLAYERS_COUNT = 2;
    final static public int ROWS = 8;
    final static public int COLS = 8;
    private Cell[][] board;
    private int whiteCount;
    private int blackCount;
    private int bittenCells;
    private Cell nowMover;
    private boolean isOver;

    public Checkers() {
        whiteCount = 12;
        blackCount = 12;
        isOver = false;
        board = new Cell[COLS][ROWS];
        bittenCells = 0;
        nowMover = Cell.WHITE;
        for (int i = 0; i < 3; i++) {
            for (int j = 0; j < ROWS; j++) {
                board[i][j] = ((i + j) % 2 == 0) ? Cell.WHITE : Cell.EMPTY;
            }
        }
        for (int i = 3; i < COLS - 3; i++) {
            for (int j = 0; j < 8; j++) {
                board[i][j] = Cell.EMPTY;
            }
        }
        for (int i = COLS - 3; i < COLS; i++) {
            for (int j = 0; j < 8; j++) {
                board[i][j] = ((i + j) % 2 == 0) ? Cell.BLACK : Cell.EMPTY;
            }
        }
    }

    private boolean isCorrectCoord(int x, int y) {
        return x >= 0 && x < COLS && y >= 0 && y < ROWS;
    }

    @Override
    public Cell getCell(int x, int y) {
        return board[x][y];
    }

    @Override
    public boolean isValid(Move move) {
        int from[] = move.getFrom();
        int to[] = move.getTo();
        if (!isCorrectCoord(from[0], from[1]) || board[from[0]][from[1]] != nowMover) {
            return false;
        }
        if (!isCorrectCoord(to[0], to[1]) || board[to[0]][to[1]] != Cell.EMPTY) {
            return false;
        }

        int diff[] = new int[2];
        diff[0] = to[0] - from[0];
        diff[1] = to[1] - from[1];

        if (Math.abs(diff[1]) == 1) {
            return (diff[0] == 1 && nowMover == Cell.WHITE || diff[0] == -1 && nowMover == Cell.BLACK) && bittenCells == 0;
        }

        Cell another = (nowMover == Cell.BLACK) ? Cell.WHITE : Cell.BLACK;
        return Math.abs(diff[0]) == 2 && Math.abs(diff[1]) == 2 && board[from[0] + diff[0] / 2][from[1] + diff[1] / 2] == another;
    }

    @Override
    public int getPlayersCount() {
        return PLAYERS_COUNT;
    }

    @Override
    public MoveResult makeMove(Move move) {
        if (isOver) {
            return MoveResult.DRAW;
        }
        if (!isValid(move)) {
            isOver = true;
            return MoveResult.LOSE;
        }

        int from[] = move.getFrom();
        int to[] = move.getTo();
        int diff[] = new int[2];
        diff[0] = to[0] - from[0];
        diff[1] = to[1] - from[1];

        if (Math.abs(diff[0]) == 2) {
            board[from[0] + diff[0] / 2][from[1] + diff[1] / 2] = Cell.EMPTY;
            if (nowMover == Cell.BLACK) {
                whiteCount--;
                if (whiteCount == 0) {
                    isOver = true;
                    return MoveResult.WIN;
                }
            } else {
                blackCount--;
                if (blackCount == 0) {
                    isOver = true;
                    return MoveResult.WIN;
                }
            }
        }
        board[from[0]][from[1]] = Cell.EMPTY;
        board[to[0]][to[1]] = nowMover;

        for (int[] i : new int[][]{{1, 1}, {1, -1}, {-1, 1}, {-1, -1}}) {
            if (isCorrectCoord(to[0] + i[0], to[1] + i[1]) && isCorrectCoord(to[0] - i[0], to[1] - i[1])) {
                if (isDifferentColours(board[to[0] + i[0]][to[1] + i[1]], board[to[0]][to[1]]) && board[to[0] - i[0]][to[1] - i[1]] == Cell.EMPTY) {
                    bittenCells++;
                }
            }
        }

        if (bittenCells > 0) {
            changePlayer();
            return MoveResult.CONTINUE;
        }

        for (int i = 0; i < COLS; i++) {
            for (int j = 0; j < ROWS; j++) {
                if (board[i][j] == nowMover) {
                    int d = (nowMover == Cell.WHITE) ? 1 : -1;
                    if ((isCorrectCoord(i + d, j + 1) && board[i + d][j + 1] == Cell.EMPTY) ||
                            (isCorrectCoord(i + d, j - 1) && board[i + d][j - 1] == Cell.EMPTY)) {
                        changePlayer();
                        return MoveResult.CONTINUE;
                    }
                }
            }
        }

        return MoveResult.WIN;
    }

    @Override
    public BoardStatus getStatus() {
        return new CheckersStatus(this);
    }

    private void changePlayer() {
        nowMover = (nowMover == Cell.BLACK) ? Cell.WHITE : Cell.BLACK;
    }

    private boolean isDifferentColours(Cell first, Cell second) {
        return first != second && first != Cell.EMPTY && second != Cell.EMPTY;
    }
}
