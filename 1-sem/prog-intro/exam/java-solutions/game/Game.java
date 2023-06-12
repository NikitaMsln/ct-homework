package game;

public class Game {
    final Player[] players;
    final boolean log;

    public Game(Player[] players, boolean log) {
        this.players = players;
        this.log = log;
    }

    public GameResult play(Board board) throws IncorrectBoardException {
        if (players.length < board.getPlayersCount()) {
            throw new IncorrectBoardException("Count of players in Board is incorrect");
        }
        while(true) {
            for (int i = 0; i < board.getPlayersCount(); i++) {
                MoveResult result = move(board, i);
                if (result != MoveResult.CONTINUE) {
                    return new GameResult(result, players[i]);
                }
            }
        }
    }

    private MoveResult move(Board board, int player) {
        Move move;
        try {
            move = players[player].makeMove(board.getStatus());
        } catch (IncorrectPlayersMove e) {
            System.out.printf("Player %s makes incorrect move: %s", players[player].getName(), e.getMessage());
            System.out.println();
            return MoveResult.LOSE;
        }
        if (board.isValid(move)) {
            if (log) {
                System.out.printf("Player %s makes move: %s", players[player].getName(), move.toString());
                System.out.println();
            }
            return board.makeMove(move);
        } else {
            if (log) {
                System.out.printf("Player %s makes incorrect move", players[player].getName());
                System.out.println();
            }
            return MoveResult.LOSE;
        }

    }
}
