package game;

public class GameResult {
    private final MoveResult result;
    private final Player lastPlayer;

    public GameResult(MoveResult result, Player lastPlayer) {
        this.result = result;
        this.lastPlayer = lastPlayer;
    }

    public MoveResult getResult() {
        return result;
    }

    public Player getLastPlayer() {
        return lastPlayer;
    }
}
