package game;

import java.util.List;
import java.util.Scanner;

public class Main {

    final private static int DEFAULT_M = 11, DEFAULT_N = 11, DEFAULT_K = 5;

    public static void main(String[] args) {
        int m, n, k;
        if (args.length < 3) {
            System.out.println("Less then 3 values for m, n and k");
            m = DEFAULT_M;
            n = DEFAULT_N;
            k = DEFAULT_K;
        } else {
            try {
                m = Integer.parseInt(args[0]);
                n = Integer.parseInt(args[1]);
                k = Integer.parseInt(args[2]);
            } catch (NumberFormatException e) {
                System.out.println("Not a numbers for m, n and k: " + e.getMessage());
                m = DEFAULT_M;
                n = DEFAULT_N;
                k = DEFAULT_K;
            }
        }
        if (m < 0 || n < 0 || k < 0 || (k > m && k > n)) {
            System.out.println("Invalid numbers for m, n and k");
            m = DEFAULT_M;
            n = DEFAULT_N;
            k = DEFAULT_K;
        }

        Scanner in = new Scanner(System.in);

        final Game game = new Game(true, List.of(
                new RandomPlayer("A", m, n),
                new RandomPlayer("B", m, n),
                new RandomPlayer("C", m, n),
                new RandomPlayer("D", m, n)
        ));
        Cell[][] borders = new Cell[m][n];
        for (int i = 0; i < m; i++) {
            for (int j = 0; j < n; j++) {
                if (i == j) {
                    borders[i][j] = Cell.B;
                } else {
                    borders[i][j] = Cell.E;
                }
            }
        }

        Tournament tour = new Tournament(m, n, k, List.of(
                new HumanPlayer("A", new Scanner(System.in), System.out),
                new RandomPlayer("B", m, n),
                new RandomPlayer("C", m, n),
                new RandomPlayer("D", m, n)
        ), false);

        System.out.println("Tour:");
        tour.hold(System.out);

        System.out.println("My game:");

        do {
            GameResult result = game.play(new MNKBoard(m, n, k, 4, borders));
            System.out.println(result);
            System.out.println("Replay? (y/n)");
            String s = in.next();
            if (!s.equalsIgnoreCase("y")) {
                break;
            }
        } while (true);
    }
}
