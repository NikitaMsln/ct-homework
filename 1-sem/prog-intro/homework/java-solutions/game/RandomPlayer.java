package game;

import java.util.Random;

public class RandomPlayer implements Player {
    private final Random random;

    private final int m, n;

    private final String name;

    public RandomPlayer(String name, final Random random, int m, int n) {
        this.random = random;
        this.m = m;
        this.n = n;
        this.name = name;
    }

    public RandomPlayer(String name, int m, int n) {
        this(name, new Random(), m, n);
    }

    public RandomPlayer(String name) {
        this(name, new Random(), 3, 3);
    }

    @Override
    public String getName() {
        return name;
    }

    @Override
    public Move move(final Position position, final Cell cell) {
        while (true) {
            int r = random.nextInt(m);
            int c = random.nextInt(n);
            final Move move = new Move(r, c, cell);
            if (position.isValid(move)) {
                return move;
            }
        }
    }
}
