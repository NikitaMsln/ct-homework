import javax.security.sasl.SaslClient;
import java.util.Scanner;

public class DTask {
    private final static int MOD = 998_244_353;

    public static void main(String[] args) {
        Scanner in = new Scanner(System.in);

        int n = in.nextInt(), k = in.nextInt();
        int R[] = new int[n+1], D[] = new int[n+1], kExp[] = new int[n+1];

        kExp[0] = 1;
        for (int i = 1; i <= n; i++) {
            kExp[i] = mul(kExp[i-1], k);
        }

        int sumOddR = 0, sumEvenR = 0;

        for (int i = 1; i <= n; i++) {
            if (i % 2 == 1) {
                sumOddR = mul(sumOddR, k);
                sumOddR = sum(sumOddR, kExp[(i + 1) / 2]);
            } else {
                sumEvenR = mul(sumEvenR, k);
                sumEvenR = sum(sumEvenR, kExp[i / 2 + 1]);
            }
            R[i] = sum(sumOddR, sumEvenR);
        }

        for (int i = 1; i <= n; i++) {
            D[i] = R[i];
            if (i > 1) {
                for (int j = 1; j * j <= i; j++) {
                    if (i % j == 0) {
                        D[i] = sum(D[i], -mul(i / j, D[j]));
                        if (j != 1 && j * j != i) {
                            D[i] = sum(D[i], -mul(j, D[i / j]));
                        }
                    }
                }
            }
        }

        int result = 0;
        D[0] = 0;
        for (int i = 1; i <= n; i++) {
            for (int j = 1; j * j <= i; j++) {
                if (i % j == 0) {
                    result = sum(result, D[j]);
                    if (j * j != i) {
                        result = sum(result, D[i / j]);
                    }
                }
            }
        }

        System.out.println(result);
    }

    public static int sum(int x, int y) {
        return (int)( ((long)x + y + MOD) % MOD);
    }

    public static int mul(int x, int y) {
        return (int)( ((long)x * y) % MOD );
    }
}