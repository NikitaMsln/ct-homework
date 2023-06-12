package game;

import java.util.Random;

public class RandomPlayer extends AbstractPlayer {
    private final Random random;
    private final int cols;
    private final int rows;

    public RandomPlayer(String name, int cols, int rows) {
        super(name);
        random = new Random();
        this.rows = rows;
        this.cols = cols;
    }

    public RandomPlayer(String name, Random random, int cols, int rows) {
        super(name);
        this.random = random;
        this.rows = rows;
        this.cols = cols;
    }

    @Override
    public Move makeMove(BoardStatus board) {
        Move move;
        do {
            int fromX = random.nextInt(cols), fromY = random.nextInt(rows), toX, toY;
            int d = (random.nextBoolean())? 1 : 2;
            toX = (random.nextBoolean())? fromX - d: fromX + d;
            toY = (random.nextBoolean())? fromY - d: fromY + d;
            move = new CheckersMove(fromX, fromY, toX, toY);
        } while (!board.isValid(move));
        return move;
    }
}
