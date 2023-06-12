package game;

import java.util.List;

public class Game {
    private final boolean log;
    private final List<Player> players;

    public Game(final boolean log, final List<Player> players) {
        this.log = log;
        this.players = players;
    }

    public GameResult play(Board board) {
        while (true) {
            for (int i = 0; i < players.size(); i++) {
                final GameResult result = move(board, players.get(i));
                if (result.getResult() != Result.UNKNOWN) {
                    return result;
                }
            }
        }
    }

    private GameResult move(final Board board, final Player player) {
        final Move move = player.move(new MovesManager(board), board.getTurn());
        final GameResult result = new GameResult(player, board.makeMove(move));
        if (log) {
            System.out.println("Player " + player.getName() + " move: " + move);
            System.out.println("Position:\n" + board);
            System.out.println(result);
        }
        return result;
    }
}