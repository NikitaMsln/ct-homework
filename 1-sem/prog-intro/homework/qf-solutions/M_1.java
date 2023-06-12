import java.util.HashMap;
import java.util.Map;
import java.util.Scanner;

public class MTask {
    public static void main(String[] args) {
        Scanner in = new Scanner(System.in);

        int t = in.nextInt();
        for (int x = 0; x < t; x++) {

            int n = in.nextInt();

            int a[] = new int[n];
            Map<Integer, Integer> C = new HashMap<>();
            for (int i = 0; i < n; i++) {
                a[i] = in.nextInt();
                C.put(a[i], 0);
            }

            int result = 0;

            for (int j = n - 1; j > 0; j--) {
                for (int i = 0; i < j; i++) {
                    result += C.getOrDefault(2*a[j] - a[i], 0);
                }
                C.put(a[j], C.get(a[j]) + 1);
            }

            System.out.println(result);
        }
    }
}