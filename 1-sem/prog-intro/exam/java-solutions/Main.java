import game.*;

public class Main {
    public static void main(String[] argv) {

        Game game = new Game(new Player[]{new RandomPlayer("A", 8, 8), new HumanPlayer("B")}, true);
        try {
            GameResult result = game.play(new Checkers());
            if (result.getResult() == MoveResult.DRAW) {
                System.out.println("Draw");
            } else if (result.getResult() == MoveResult.LOSE) {
                System.out.printf("%s lose", result.getLastPlayer().getName());
                System.out.println();
            } else {
                System.out.printf("%s win", result.getLastPlayer().getName());
                System.out.println();
            }
        } catch (IncorrectBoardException e) {
            System.out.println(e.getMessage());
        }

        Player[] players = new Player[4];
        players[0] = new RandomPlayer("A", 8, 8);
        players[1] = new RandomPlayer("B", 8, 8);
        players[2] = new RandomPlayer("C", 8, 8);
        players[3] = new RandomPlayer("D", 8, 8);
        CheckersTournament tournament = new CheckersTournament(players);
        MoveResult[][] tourResult = tournament.hold();

        for (int i = 0; i < 4; i++) {
            for (int j = 0; j < 4; j++) {
                String result;
                if (tourResult[i][j] == MoveResult.LOSE) {
                    result = "lose";
                } else if (tourResult[i][j] == MoveResult.WIN) {
                    result = "win";
                } else {
                    result = "draw";
                }
                System.out.printf("Game %d: %s %s %s", i * 4 + j + 1, players[i].getName(), players[j].getName(), result);
            }
        }
    }
}
