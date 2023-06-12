package game;

import java.util.Map;

public class MNKBoard implements Board {
    private final int m, n, k;
    private static final Map<Cell, Character> SYMBOLS = Map.of(
        Cell.X, 'X',
        Cell.O, 'O',
        Cell.H, '-',
        Cell.V, '|',
        Cell.B, '#',
        Cell.E, '.'
    );

    private static final Cell[] order = new Cell[]{Cell.X, Cell.O, Cell.H, Cell.V};

    private static final int[][] LINE_DIRECTION = new int[][]{
        {1, 0},
        {0, 1},
        {1, 1}
    };

    private final Cell[][] cells;
    private int turn;

    private final int playersCount;

    private int emptyCells;

    public MNKBoard(int m, int n, int k, int playersCount, Cell[][] borders) {
        this.m = m;
        this.n = n;
        this.k = k;
        emptyCells = m * n;
        if (playersCount < 0 || playersCount > 4) {
            throw new IndexOutOfBoundsException("Max players count is 4");
        }
        this.playersCount = playersCount;
        cells = new Cell[m][n];
        for (int r = 0; r < m; r++) {
            for (int c = 0; c < n; c++) {
                if (borders[r][c] != Cell.B) {
                    cells[r][c] = Cell.E;
                } else {
                    cells[r][c] = Cell.B;
                    emptyCells--;
                }
            }
        }
        turn = 0;
    }

    @Override
    public Cell getTurn() {
        return order[turn];
    }

    @Override
    public Cell getCell(final int r, final int c) {
        return cells[r][c];
    }

    @Override
    public boolean isValid(Move move) {
        return 0 <= move.getRow() && move.getRow() < m
            && 0 <= move.getColumn() && move.getColumn() < n
            && cells[move.getRow()][move.getColumn()] == Cell.E
            && order[turn] == move.getValue();
    }

    @Override
    public Result makeMove(Move move) {
        if (!isValid(move)) {
            return Result.LOSE;
        }

        cells[move.getRow()][move.getColumn()] = order[turn];
        turn = (turn + 1) % playersCount;
        emptyCells--;

        for (int[] vector : LINE_DIRECTION) {
            int before = 0, after = 0;
            for (
                int nowRow = move.getRow() - vector[0], nowColumn = move.getColumn() - vector[1];
                nowRow >= 0 && nowColumn >= 0 && cells[nowRow][nowColumn] == move.getValue() && before + after + 1 < k;
                nowRow -= vector[0], nowColumn -= vector[1], before++
            )
                ;

            for (
                int nowRow = move.getRow() + vector[0], nowColumn = move.getColumn() + vector[1];
                nowRow < m && nowColumn < n && cells[nowRow][nowColumn] == move.getValue() && before + after + 1 < k;
                nowRow += vector[0], nowColumn += vector[1], after++
            )
                ;

            if (before + after + 1 >= k) {
                return Result.WIN;
            }
        }

        if (emptyCells <= 0) {
            return Result.DRAW;
        }

        return Result.UNKNOWN;
    }

    @Override
    public String toString() {
        final StringBuilder sb[] = new StringBuilder[m + 1];
        sb[0] = new StringBuilder();
        sb[0].append('\t');
        for (int r = 1; r <= m; r++) {
            sb[r] = new StringBuilder();
            sb[r].append(r);
            sb[r].append('\t');
        }
        for (int c = 0; c < n; c++) {
            sb[0].append(c + 1);
            sb[0].append('\t');
            for (int r = 1; r <= m; r++) {
                sb[r].append(SYMBOLS.get(cells[r - 1][c]));
                sb[r].append('\t');
            }
        }
        StringBuilder res = new StringBuilder();
        for (int i = 0; i <= m; i++) {
            res.append(sb[i]);
            res.append(System.lineSeparator());
        }
        return res.toString();
    }
}
