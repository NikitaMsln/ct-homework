import java.util.Scanner;

public class BTask {
    public static void main(String[] args) {
        Scanner in = new Scanner(System.in);
        int n = in.nextInt();
        for (int i = 0; i < n; i++) {
            System.out.println(710 * (i - 25_000));
        }
    }
}
