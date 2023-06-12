package game;

import java.io.PrintStream;
import java.util.Scanner;

public class HumanPlayer implements Player {
    private final PrintStream out;
    private final Scanner in;

    private final String name;

    public HumanPlayer(String name, Scanner in, PrintStream out) {
        this.in = in;
        this.out = out;
        this.name = name;
    }

    @Override
    public String getName() {
        return name;
    }

    @Override
    public Move move(final Position position, final Cell cell) {
        out.println("Board:");
        out.println(position.getBoard());
        out.println(cell + "'s move");
        out.println("Enter row and column");
        Move move;
        while (true) {
            String row = in.next(), column = in.next();
            try {
                move = new Move(Integer.parseInt(row) - 1, Integer.parseInt(column) - 1, cell);
            } catch (NumberFormatException e) {
                out.println("Error: Enter numbers for row and column");
                continue;
            }
            if (position.isValid(move)) {
                break;
            } else {
                out.println("Incorrect move, enter row and column again");
            }
        }
        return move;
    }
}

