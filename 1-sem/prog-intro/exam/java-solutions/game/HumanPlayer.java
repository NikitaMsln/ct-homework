package game;

import java.util.Map;
import java.util.Scanner;

public class HumanPlayer extends AbstractPlayer {
    final private Scanner in;
    final private Map<Cell, String> values = Map.of(
            Cell.EMPTY, ".",
            Cell.BLACK, "b",
            Cell.WHITE, "w"
    );

    public HumanPlayer(String name, Scanner in) {
        super(name);
        this.in = in;
    }

    public HumanPlayer(String name) {
        super(name);
        in = new Scanner(System.in);
    }

    @Override
    public Move makeMove(BoardStatus board) throws IncorrectPlayersMove {
        Cell[][] cells = board.getBoard();
        for (int i = 0; i < cells[0].length; i++) {
            System.out.printf("\t%d", i);
        }
        System.out.println();
        for (int i = 0; i < cells.length; i++) {
            System.out.printf("%d", i);
            for (int j = 0; j < cells[0].length; j++) {
                System.out.printf("\t%s", values.get(cells[i][j]));
            }
            System.out.println();
        }
        Move move;
        while (true) {
            int[] from = new int[2], to = new int[2];
            System.out.println("Enter cell from:");
            for (int i = 0; i < 2; i++) {
                if (in.hasNextInt()) {
                    from[i] = in.nextInt();
                } else {
                    throw new IncorrectPlayersMove("Input in human player ended");
                }
            }
            System.out.println("Enter cell to:");
            for (int i = 0; i < 2; i++) {
                if (in.hasNextInt()) {
                    to[i] = in.nextInt();
                } else {
                    throw new IncorrectPlayersMove("Input in human player ended");
                }
            }
            move = new CheckersMove(from[0], from[1], to[0], to[1]);
            if (board.isValid(move)) {
                break;
            } else {
                System.out.println("Invalid move");
            }
        }
        return move;
    }
}
