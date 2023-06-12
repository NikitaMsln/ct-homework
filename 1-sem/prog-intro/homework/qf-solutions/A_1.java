import java.util.Scanner;

public class ATask {
    public static void main(String[] args) {
        Scanner in = new Scanner(System.in);
        int a = in.nextInt(), b = in.nextInt(), n = in.nextInt();
        System.out.println((n - a - 1) / (b - a) * 2 + 1);
    }
}
