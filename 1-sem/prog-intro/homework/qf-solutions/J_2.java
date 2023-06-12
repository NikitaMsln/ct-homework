import java.util.Scanner;

public class JTask {
    public static void main(String[] args) {
        Scanner in = new Scanner(System.in);
        int n = in.nextInt();
        int[][] ways = new int[n][n];
        int result[][] = new int[n][n];
        for (int i = 0; i < n; i++) {
            String line = in.next();
            for (int j = 0; j < n; j++) {
                ways[i][j] = Character.getNumericValue(line.charAt(j));
            }
        }

        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                if (ways[i][j] == 0) {
                    result[i][j] = 0;
                    continue;
                } else {
                    result[i][j] = 1;
                    for (int k = 0; k < n; k++) {
                        ways[i][k] = (ways[i][k] - ways[j][k] + 10) % 10;
                    }
                }
            }
        }

        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                System.out.printf("%d", result[i][j]);
            }
            System.out.println();
        }
    }
}
