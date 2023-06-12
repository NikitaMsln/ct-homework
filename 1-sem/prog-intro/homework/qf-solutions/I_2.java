import java.util.Scanner;

public class ITask {
    public static void main(String[] args) {
        Scanner in = new Scanner(System.in);

        int n = in.nextInt();
        int xl = Integer.MAX_VALUE, yl = Integer.MAX_VALUE, xr = Integer.MIN_VALUE, yr = Integer.MIN_VALUE;
        for (int i = 0; i < n; i++) {
            int x = in.nextInt(), y = in.nextInt(), h = in.nextInt();
            if (x - h < xl) {
                xl = x - h;
            }
            if (x + h > xr) {
                xr = x + h;
            }
            if (y - h < yl) {
                yl = y - h;
            }
            if (y + h > yr) {
                yr = y + h;
            }
        }

        System.out.printf("%d %d %d", (xl + xr) / 2, (yl + yr) / 2, (Math.max(xr - xl, yr - yl) + 1) / 2);
        System.out.println();
    }
}
