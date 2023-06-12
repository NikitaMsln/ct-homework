package game;

public class CheckersTournament {
    private final Player[] players;

    // :NOTE: - Массивы вместо списков
    public CheckersTournament(Player[] players) {
        this.players = players;
    }

    public MoveResult[][] hold() {
        MoveResult[][] result = new MoveResult[players.length][players.length];
        for (int i = 0; i < players.length; i++) {
            for (int j = 0; j < players.length; j++) {
                if (i == j) {
                    result[i][j] = MoveResult.DRAW;
                } else {
                    Game game = new Game(new Player[]{players[i], players[j]}, false);
                    try {
                        GameResult res = game.play(new Checkers());
                        if (res.getResult() == MoveResult.DRAW) {
                            result[i][j] = MoveResult.DRAW;
                            // :NOTE: - Сравнение сторок на ==
                        } else if (res.getLastPlayer().getName() == players[i].getName()) {
                            result[i][j] = res.getResult();
                        } else {
                            result[i][j] = (res.getResult() == MoveResult.LOSE)? MoveResult.WIN : MoveResult.LOSE;
                        }
                    } catch (IncorrectBoardException e) {
                        System.out.println(e.getMessage());
                        result[i][j] = MoveResult.DRAW;
                    }
                }
            }
        }
        return result;
    }
}
