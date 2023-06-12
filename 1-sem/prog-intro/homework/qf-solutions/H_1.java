import java.util.Scanner;

public class HTask {
    public static void main(String[] args) {
        Scanner in = new Scanner(System.in);

        int n = in.nextInt();
        int a[] = new int[n], A[] = new int[n+1];
        int maxTransaction  = Integer.MIN_VALUE;
        A[0] = 0;
        for (int i = 0; i < n; i++) {
            a[i] = in.nextInt();
            A[i+1] = A[i] + a[i];
            if (a[i] > maxTransaction) {
                maxTransaction = a[i];
            }
        }

        int f[] = new int[A[n] + 1], queriesInNowTransaction = 1;
        f[0] = 0;
        for (int i = 1; i <= A[n]; i++) {
            queriesInNowTransaction++;
            if (queriesInNowTransaction > a[f[i-1]]) {
                f[i] = f[i-1] + 1;
                queriesInNowTransaction = 1;
            } else {
                f[i] = f[i-1];
            }
        }

        int results[] = new int[A[n] + 1];
        for (int i = maxTransaction; i <= A[n]; i++) {
            results[i] = 1;
            for (int b = 0; b + i < A[n]; results[i]++) {
                b = A[f[b + i]];
            }
        }

        int q = in.nextInt();
        for (int i = 0; i < q; i++) {
            int t = in.nextInt();

            if (t < maxTransaction) {
                System.out.println("Impossible");
                continue;
            } else if (t >= A[n]) {
                System.out.println(1);
            } else {
                System.out.println(results[t]);
            }
        }
    }
}
