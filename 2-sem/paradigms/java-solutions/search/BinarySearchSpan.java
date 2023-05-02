package search;

public class BinarySearchSpan {

    public interface Condition {
        boolean check(int x);
    }

    // Define: a - array, n -size a, x - searched element, i0 - min i: a[i] >= x, i1 - min i: a[i] > x, cnt - count i: a[i] = x
    // Pre: forall 0 < i < n: a[i] >= a[i-1] && x = args[0] && a[0...] = args[1...] && args.length > 0 && args is parsable integers
    // Post: print i0 and cnt
    public static void main(String[] args) {
        // Pre: forall 0 < i < n: a[i] >= a[i-1] && x = args[0] && a[0...] = args[1...] && args.length > 0 && args is parsable integers
        int n = args.length - 1;
        // n = size a && forall 0 < i < n: a[i] >= a[i-1] && x = args[0] && a[0...] = args[1...] && args.length > 0 && args is parsable integers
        int elem = Integer.parseInt(args[0]);
        // elem = x && n = size a && forall 0 < i < n: a[i] >= a[i-1] && a[0...] = args[1...] && args.length > 0 && args is parsable integers
        int[] array = new int[n];
        int j = 0;
        // j = 0 && elem = x && n = size a && forall 0 < i < n: a[i] >= a[i-1] && a[0...] = args[1...] && args.length > 0 && args is parsable integers
        // Inv: j <= n && forall 0 <= i < j: array[i] = a[i] && elem = x && n = size a && forall 0 < i < n: a[i] >= a[i-1] && a[0...] = args[1...] && args.length > 0 && args is parsable integers
        while (j != n) {
            // j < n && forall 0 <= i < j: array[i] = a[i] && elem = x && n = size a && forall 0 < i < n: a[i] >= a[i-1] && a[0...] = args[1...] && args.length > 0 && args is parsable integers
            String s = args[j + 1];
            // s = args[j + 1] = a[j] && j < n && forall 0 <= i < j: array[i] = a[i] && elem = x && n = size a && forall 0 < i < n: a[i] >= a[i-1] && a[0...] = args[1...] && args.length > 0 && args is parsable integers
            array[j] = Integer.parseInt(s);
            // j + 1 <= n && forall 0 <= i < j + 1: array[i] = a[i] && elem = x && n = size a && forall 0 < i < n: a[i] >= a[i-1] && a[0...] = args[1...] && args.length > 0 && args is parsable integers
            j = j + 1;
            // j <= n && forall 0 <= i < j: array[i] = a[i] && elem = x && n = size a && forall 0 < i < n: a[i] >= a[i-1] && a[0...] = args[1...] && args.length > 0 && args is parsable integers
        }
        // j = n && forall 0 <= i < j: array[i] = a[i] && elem = x && n = size a && forall 0 < i < n: a[i] >= a[i-1] && args is parsable integers
        // array = a && elem = x && n = size a && forall 0 < i < n: a[i] >= a[i-1]
        int left = recursiveBinarySearch(array, 0, array.length, (x) -> x >= elem);
        // left = i0 && forall 0 < i < n: a[i] >= a[i-1]
        int right = iterativeBinarySearch(array, (x) -> x > elem);
        // left = i0 && right = i1 && forall 0 < i < n: a[i] >= a[i-1]
        // left = i0 && right = i1 && i0 - min i: a[i] >= x, i1 - max i: a[i] < x, cnt - count i: a[i] = x
        // left = i0 && right = i1 && cnt = i1 - i0 + 1
        System.out.println(left + " " + (right - left));
        // printed i0 and cnt
    }

    // Pre: exist i0: forall 0 <= i < i0 condition is true and forall i0 <= i <= n condition is false
    // Post: return i0
    public static int iterativeBinarySearch(int[] a, Condition condition) {
        // Pre: exist i0: forall 0 <= i < i0 condition is true and forall i0 <= i <= n condition is false
        int left = 0, right = a.length;
        // left = 0 && right = n && forall 0 <= i < i0 condition is true and forall i0 <= i <= n condition is false
        // Inv: 0 <= left <= i0 <= right <= n && forall 0 <= i < i0 condition is true and forall i0 <= i <= n condition is false
        while (left != right) {
            // 0 <= left <= i0 <= right <= n && forall 0 <= i < i0 condition is true and forall i0 <= i <= n condition is false
            int middle = left + (right - left) / 2;
            // left <= middle <= right && 0 <= left <= i0 <= right <= n && forall 0 <= i < i0 condition is true and forall i0 <= i <= n condition is false
            if (condition.check(a[middle])) {
                // 0 <= left <= i0 <= middle <= right <= n && forall 0 <= i < i0 condition is true and forall i0 <= i <= n condition is false
                right = middle;
                // 0 <= left <= i0 <= right <= n && forall 0 <= i < i0 condition is true and forall i0 <= i <= n condition is false
            } else {
                // 0 <= left <= middle < i0 <= right <= n && forall 0 <= i < i0 condition is true and forall i0 <= i <= n condition is false
                left = middle + 1;
                // 0 <= left <= i0 <= right <= n && forall 0 <= i < i0 condition is true and forall i0 <= i <= n condition is false
            }
            // 0 <= left <= i0 <= right <= n && forall 0 <= i < i0 condition is true and forall i0 <= i <= n condition is false
        }
        // left = right - 1 && 0 <= left <= i0 <= right <= n && && forall 0 <= i < i0 condition is true and forall i0 <= i <= n condition is false
        // right <= i0 <= right => i0 = right
        return right;
        // return i0
    }

    // Pre: exist i0: forall 0 <= i < i0 condition is true and forall i0 <= i <= n condition is false && 0 <= left <= i0 <= right <= n
    // Post: i0
    public static int recursiveBinarySearch(int[] a, int left, int right, Condition condition) {
        int result;
        // forall 0 <= i < i0 condition is true and forall i0 <= i <= n condition is false && 0 <= left <= i0 <= right <= n
        if (left == right) {
            // forall 0 <= i < i0 condition is true and forall i0 <= i <= n condition is false && 0 <= left <= i0 <= right <= n && left = right
            // right <= i0 <= right
            result = right;
            // result = i0
        } else {
            // forall 0 <= i < i0 condition is true and forall i0 <= i <= n condition is false && 0 <= left <= i0 <= right <= n
            int middle = left + (right - left) / 2;
            // left <= middle <= right && 0 <= left <= i0 <= right <= n && forall 0 <= i < i0 condition is true and forall i0 <= i <= n condition is false
            if (condition.check(a[middle])) {
                // 0 <= left <= i0 <= middle <= right <= n && forall 0 <= i < i0 condition is true and forall i0 <= i <= n condition is false
                result = recursiveBinarySearch(a, left, middle, condition);
                // result = i0
            } else {
                // -1 <= left <= middle < i0 <= right <= n && forall 0 <= i < i0 condition is true and forall i0 <= i <= n condition is false
                result = recursiveBinarySearch(a, middle + 1, right, condition);
                // result = i0
            }
            // result = i0
        }
        // result = i0
        return result;
        // return i0
    }
}
