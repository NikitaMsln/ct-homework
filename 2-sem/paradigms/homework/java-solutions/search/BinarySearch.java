package search;

public class BinarySearch {
    // Define: (a - array, n - size of a, x - search element, i0 - min i: a[i] <= x)
    // Pre: forall 0 < i < n: a[i-1] >= a[i] && x = args[0] && a[0...] = args[1...] && args.length > 0 && args is parsable integers
    // Post: print i0
    public static void main(String[] args) {
        // Pre: forall 0 < i < n: a[i-1] >= a[i] && x = args[0] && a[0...] = args[1...] && args.length > 0 && n = args.length - 1 && args is parsable integers
        int n = args.length - 1;
        // forall 0 < i < n: a[i-1] >= a[i] && x = args[0] && a[0...] = args[1...] && args.length > 0 && n = args.length - 1 && args is parsable integers
        int elem = Integer.parseInt(args[0]);
        // forall 0 < i < n: a[i-1] >= a[i] && x = elem && a[0...] = args[1...] && args.length > 0 && n = args.length - 1 && args is parsable integers
        int[] array = new int[n];
        int j = 0;
        // j = 0 && forall 0 < i < n: a[i-1] >= a[i] && x = elem && a[0...] = args[1...] && n >= 0 && args is parsable integers
        // Inv: forall 0 <= i < j <= n: array[i] = a[i] && x = elem && a[0...] = args[1...] && n >= 0 && forall 0 < i < n: a[i-1] >= a[i] && args is parsable integers
        while (j != n) {
            // Pre: forall 0 <= i < j < n: array[i] = a[i] && x = elem && a[0...] = args[1...] && n >= 0 && forall 0 < i < n: a[i-1] >= a[i] && args is parsable integers
            String s = args[j + 1];
            // s = args[j + 1] = a[j] && forall 0 <= i < j < n: array[i] = a[i] && x = elem && a[0...] = args[1...] && n >= 0 && forall 0 < i < n: a[i-1] >= a[i] && args is parsable integers
            array[j] = Integer.parseInt(s);
            // array[j] = a[j] && forall 0 <= i < j < n: array[i] = a[i] && x = elem && a[0...] = args[1...] && n >= 0 && forall 0 < i < n: a[i-1] >= a[i] && args is parsable integers
            // forall 0 <= i < j + 1 <= n: array[i] = a[i] && x = elem && a[0...] = args[1...] && n >= 0 && forall 0 < i < n: a[i-1] >= a[i] && args is parsable integers
            j = j + 1;
            // forall 0 <= i < j <= n: array[i] = a[i] && x = elem && a[0...] = args[1...] && n >= 0 && forall 0 < i < n: a[i-1] >= a[i] && args is parsable integers
        }
        // Post: j = n && forall 0 <= i < j <= n: array[i] = a[i] && x = elem && a[0...] = args[1...] && n >= 0 && forall 0 < i < n: a[i-1] >= a[i] && args is parsable integers
        // array = a && x = elem && forall 0 < i < n: a[i-1] >= a[i] && n >= 0
        int result = iterativeBinarySearch(array, elem);
        // result = i0 && n >= 0
        System.out.println(result);
        // printed i0
    }

    // Pre: define: (n = a.length, i0 = min i: a[i] <= x); forall 0 < i < n: a[i-1] >= a[i] && 0 <= i0 <= n
    // Post: return i0
    public static int iterativeBinarySearch(int[] a, int x) {
        // Pre: forall 0 < i < n: a[i-1] >= a[i] && 0 <= i0 <= n
        int left = -1, right = a.length;
        // forall 0 < i < n: a[i-1] >= a[i] && left = -1 && right = n
        // Inv: left < i0 <= right && forall 0 < i < n: a[i-1] >= a[i]
        while (left != right - 1) {
            // Inv: -1 <= left < i0 <= right <= n && forall 0 < i < n: a[i-1] >= a[i]
            int middle = left + (right - left) / 2;
            // left <= middle <= right && -1 <= left < i0 <= right <= n && forall 0 < i < n: a[i-1] >= a[i]
            if (a[middle] <= x) {
                // left <= middle <= right && a[middle] <= x && -1 <= left < i0 <= right <= n && forall 0 < i < n: a[i-1] >= a[i]
                // (left <= middle <= right && a[middle] <= x && forall 0 < i < n: a[i-1] >= a[i]) => (forall middle < i <= right: i0 < i && forall 0 < i < n: a[i-1] >= a[i]) => i0 <= middle
                // Pre: -1 <= left < i0 <= middle <= right <= n && forall 0 < i < n: a[i-1] >= a[i]
                right = middle;
                // Pre: -1 <= left < i0 <= middle <= right <= n && forall 0 < i < n: a[i-1] >= a[i]
            } else {
                // left <= middle <= right && a[middle] > x && left < i0 <= right && forall 0 < i < n: a[i-1] >= a[i]
                // (left <= middle <= right && a[middle] > x && forall 0 < i < n: a[i-1] >= a[i]) => (forall left < i <= middle: i0 > i && forall 0 < i < n: a[i-1] >= a[i]) => i0 > middle
                // Pre: -1 <= left < middle < i0 <= right <= n && forall 0 < i < n: a[i-1] >= a[i]
                left = middle;
                // Post: left < i0 <= right && forall 0 < i < n: a[i-1] >= a[i]
            }
        }
        // Post: left = right - 1; -1 <= left < i0 <= right <= n && forall 0 < i < n: a[i-1] >= a[i]
        // (right - 1 < i0 <= right) => (right <= i0 <= right) => right = i0
        // right = i0
        return right;
    }

    // Pre: define: (n = a.length, i0 = min i: a[i] <= x); forall 0 < i < n: a[i-1] >= a[i] && -1 <= left < i0 <= right <= n
    // Post: return i0
    public static int recursiveBinarySearch(int[] a, int x, int left, int right) {
        int result;
        // Pre: forall 0 < i < n: a[i-1] >= a[i] && -1 <= left < i0 <= right <= n
        if (left == right - 1) {
            // Pre: (left < i0 <= right && left = right - 1) => (right - 1 < i0 <= right) => (right <= i0 <= right) => (i0 = right)
            result = right;
            // Post: result = i0
        } else {
            // Pre: left != right - 1 && -1 <= left < i0 <= right <= n && forall 0 < i < n: a[i-1] >= a[i]
            int middle = left + (right - left) / 2;
            // left <= middle <= right && -1 <= left < i0 <= right <= n && forall 0 < i < n: a[i-1] >= a[i]
            if (a[middle] <= x) {
                // Pre: a[middle] <= x && left <= middle <= right && left < i0 <= right <= n && forall 0 < i < n: a[i-1] >= a[i]
                // (a[middle] <= x && left <= middle <= right && forall 0 < i < n: a[i-1] >= a[i]) => (forall middle < i <= right: i0 < i) => (i0 <= middle)
                // Pre: -1 <= left < x <= middle <= right <= n && forall 0 < i < n: a[i-1] >= a[i]
                result = recursiveBinarySearch(a, x, left, middle);
                // Post: result = i0
            } else {
                // Pre: a[middle] > x; -1 <= left <= middle <= right && left < i0 <= right <= n && forall 0 < i < n: a[i-1] >= a[i]
                // (-1 <= left <= middle <= right && a[middle] > x && forall 0 < i < n: a[i-1] >= a[i]) => (forall left < i <= middle: i0 > i && forall 0 < i < n: a[i-1] >= a[i]) => (i0 > middle)
                // Pre: -1 <= left < middle < i0 <= right <= n && forall 0 < i < n: a[i-1] >= a[i]
                result = recursiveBinarySearch(a, x, middle, right);
                // Post: result = i0
            }
            // Post: result = i0
        }
        // Post: result = i0
        return result;
    }
}
