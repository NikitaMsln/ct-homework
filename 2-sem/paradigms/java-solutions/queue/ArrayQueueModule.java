package queue;

/*
Model: a[1]...a[n]
Invariant: forall 1 <= i <= n: a[i] != null
Let immutable(n): n >= 0 && for i=1...n: a'[i] = a[i]
*/
public class ArrayQueueModule {
    private static int size = 0;
    private static int first = 0;
    private static int last = 0;
    private static Object[] elements = new Object[5];

    // Pre: 0 <= index < n && element != null
    // Post: n' = n && for i in {1...n} \ {index}: a'[i] = a[i] && a[index] = element
    public static void set(int index, Object element) {
        assert index >= 0 && index < size && element != null;
        index = modSum(index, first);
        elements[index] = element;
    }

    // Pre: 0 <= index < n
    // Post: return a[index + 1] && n' = n && immutable(n)
    public static Object get(int index) {
        assert index >= 0 && index < size;
        index = modSum(index, first);
        return elements[index];
    }

    // Pre: element != null
    // Post: n' = n + 1 && n >= 0 && for i=2...n: a'[i-1] = a[i] && a'[1] = element
    public static void push(Object element) {
        assert element != null;
        ensureCapacity();
        size++;
        first = modSub(first, 1);
        elements[first] = element;
        if (size == 1) {
            last = first;
        }
    }

    // Pre: n > 0
    // Post: return a[n] && immutable(n) && n' = n
    public static Object peek() {
        assert size > 0;
        return elements[last];
    }

    // Pre: n > 0
    // Post: return a[1] && n' = n - 1 && forall 1 <= i < n: a'[i] = a[i]
    public static Object remove() {
        assert size > 0;
        size--;
        Object value = elements[last];
        elements[last] = null;
        last = modSub(last, 1);
        return value;
    }

    // Pre: element != null
    // Post: n' = n + 1 && immutable(n) && a'[n'] = element
    public static void enqueue(Object element) {
        assert element != null;
        ensureCapacity();
        size++;
        last = modSum(last, 1);
        elements[last] = element;
        if (size == 1) {
            first = last;
        }
    }

    // Pre: n > 0
    // Post: return a[1] && n' = n && immutable(n)
    public static Object element() {
        assert size > 0;
        return elements[first];
    }

    // Pre: n > 0
    // Post: return a[1] && n' = n - 1 && forall 1 <= i < n: a'[i] = a[i + 1]
    public static Object dequeue() {
        assert size > 0;
        size--;
        Object value = elements[first];
        elements[first] = null;
        first = modSum(first, 1);
        return value;
    }

    // Post: return n && immutable(n) && n' = n
    public static int size() {
        return size;
    }

    // Post: return n == 0 && immutable(n) && n' = n
    public static boolean isEmpty() {
        return size == 0;
    }

    // Post: n' = 0 && a' = {}
    public static void clear() {
        size = 0;
        first = 0;
        last = 0;
        elements = new Object[5];
    }

    private static void ensureCapacity() {
        if (size < elements.length) {
            return;
        }

        Object[] newElements = new Object[elements.length * 2];
        for (int i = 0, j = first; i < size; i++, j++) {
            if (j == elements.length) {
                j = 0;
            }
            newElements[i] = elements[j];
        }
        first = 0;
        last = elements.length - 1;
        elements = newElements;
    }

    private static int modSum(int first, int second) {
        first += second;
        if (first >= elements.length) {
            return first - elements.length;
        } else {
            return first;
        }
    }

    private static int modSub(int first, int second) {
        first -= second;
        if (first < 0) {
            return elements.length + first;
        } else {
            return first;
        }
    }
}
