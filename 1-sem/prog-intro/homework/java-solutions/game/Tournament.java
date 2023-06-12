package game;

import java.io.PrintStream;
import java.util.Arrays;
import java.util.List;

public class Tournament {
    final private List<Player> players;
    final private Cell[][] borders;
    final private int m, n, k;

    final private static int DRAW = -1;

    final private boolean printGames;

    Tournament(int m, int n, int k, List<Player> players, boolean printGames, Cell[][] borders) {
        this.players = players;
        this.borders = borders;
        this.m = m;
        this.n = n;
        this.k = k;
        this.printGames = printGames;
    }

    Tournament(int m, int n, int k, List<Player> players, boolean printGames) {
        Cell[][] noBorders = new Cell[m][n];
        for (int i = 0; i < m; i++) {
            for (int j = 0; j < n; j++) {
                noBorders[i][j] = Cell.E;
            }
        }
        this.players = players;
        this.borders = noBorders;
        this.m = m;
        this.n = n;
        this.k = k;
        this.printGames = printGames;
    }

    private int play(int player1, int player2) {
        if (printGames) {
            System.out.printf("Player %s vs player %s:\n", players.get(player1).getName(), players.get(player2).getName());
        }
        Game game = new Game(printGames, List.of(
                players.get(player1),
                players.get(player2)
        ));
        GameResult result = game.play(new MNKBoard(m, n, k, 2, borders));
        if (result.getResult() == Result.DRAW) {
            return DRAW;
        } else {
            Player player = result.getPlayer();
            boolean isWin = result.getResult() == Result.WIN, isFirstPlayer = player == players.get(player1);
            if (isWin && isFirstPlayer || !isWin && !isFirstPlayer) {
                return player1;
            } else {
                return player2;
            }
        }
    }

    public void hold(PrintStream out) {
        String[][] results = new String[players.size()][players.size()];
        int[][] points = new int[players.size()][2];
        for (int i = 0; i < points.length; i++) {
            points[i][0] = i;
            points[i][1] = 0;
        }
        for (int i = 0; i < players.size(); i++) {
            for (int j = 0; j < players.size(); j++) {
                if (i == j) {
                    results[i][j] = "";
                } else {
                    int result = play(i, j);
                    if (result == DRAW) {
                        results[i][j] = "/";
                        points[i][1]++;
                        points[j][1]++;
                    } else {
                        results[i][j] = players.get(result).getName();
                        points[result][1] += 3;
                    }
                }
            }
        }

        out.printf("\t");
        for (int i = 0; i < players.size(); i++) {
            out.printf("%s\t", players.get(i).getName());
        }
        out.println();
        for (int i = 0; i < players.size(); i++) {
            out.printf("%s\t", players.get(i).getName());
            for (int j = 0; j < players.size(); j++) {
                out.printf("%s\t", results[i][j]);
            }
            out.println();
        }

        Arrays.sort(points, (int[] x, int[] y) -> y[1] - x[1]);
        for (int i = 0; i < points.length; i++) {
            out.printf("%s:\t%d\n", players.get(points[i][0]).getName(), points[i][1]);
        }

    }
}
