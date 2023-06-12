package game;

public class GameResult {
    final private Player player;
    final private Result result;

    GameResult(Player player, Result result) {
        this.player = player;
        this.result = result;
    }

    public Player getPlayer() {
        return player;
    }

    public Result getResult() {
        return result;
    }

    public String toString() {
        if (result == Result.WIN) {
            return "Player " + player.getName() + " won";
        } else if (result == Result.LOSE) {
            return "Player " + player.getName() + " lose";
        } else if (result == Result.DRAW){
            return "Draw";
        } else {
            return "Continue";
        }
    }
}
