package expression.generic;

public class Main {
    public static void main(String[] args) {
        if (args.length < 2) {
            System.out.println("Enter type of numbers and expression");
            return;
        } else if (args[0].length() == 0 || args[0].charAt(0) != '-') {
            System.out.println("Mode must start at '-'");
            return;
        }

        Object[][][] result;
        try {
            result = (new GenericTabulator()).tabulate(args[0].substring(1), args[1], -2, 2, -2, 2, -2 , 2);
        } catch (Exception e) {
            System.out.println("Parsing expression error: " + e.getMessage());
            return;
        }

        for (int x = -2; x <= 2; x++) {
            for (int y = -2; y <= 2; y++) {
                for (int z = -2; z <= 2; z++) {
                    String evaluateResult = (result[x+2][y+2][z+2] != null)? result[x+2][y+2][z+2].toString() : "Evaluation error";
                    System.out.printf("x = %d; y = %d; z = %d:\t\t%s", x, y, z, evaluateResult);
                    System.out.println();
                }
            }
        }
    }
}
