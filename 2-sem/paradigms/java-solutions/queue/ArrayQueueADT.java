package queue;

/*
Model: a[1]...a[n]
Invariant: forall 1 <= i <= n: a[i] != null
Let immutable(n): n >= 0 && for i=1...n: a'[i] = a[i]
*/
public class ArrayQueueADT {
    private int size = 0;
    private int first = 0;
    private int last = 0;
    private Object[] elements = new Object[5];

    // Pre: 0 <= index < n && element != null
    // Post: n' = n && for i in {1...n} \ {index}: a'[i] = a[i] && a[index] = element
    public static void set(ArrayQueueADT queue, int index, Object element) {
        assert index >= 0 && index < queue.size && element != null;
        index = modSum(queue, index, queue.first);
        queue.elements[index] = element;
    }

    // Pre: 0 <= index < n
    // Post: return a[index + 1] && n' = n && immutable(n)
    public static Object get(ArrayQueueADT queue, int index) {
        assert index >= 0 && index < queue.size;
        index = modSum(queue, index, queue.first);
        return queue.elements[index];
    }

    // Pre: element != null
    // Post: n' = n + 1 && n >= 0 && for i=2...n: a'[i-1] = a[i] && a'[1] = element
    public static void push(ArrayQueueADT queue, Object element) {
        assert element != null;
        ensureCapacity(queue);
        queue.size++;
        queue.first = modSub(queue, queue.first, 1);
        queue.elements[queue.first] = element;
        if (queue.size == 1) {
            queue.last = queue.first;
        }
    }

    // Pre: n > 0
    // Post: return a[n] && immutable(n) && n' = n
    public static Object peek(ArrayQueueADT queue) {
        assert queue.size > 0;
        return queue.elements[queue.last];
    }

    // Pre: n > 0
    // Post: return a[1] && n' = n - 1 && forall 1 <= i < n: a'[i] = a[i]
    public static Object remove(ArrayQueueADT queue) {
        assert queue.size > 0;
        queue.size--;
        Object value = queue.elements[queue.last];
        queue.elements[queue.last] = null;
        queue.last = modSub(queue, queue.last, 1);
        return value;
    }

    // Post: return empty queue
    public static ArrayQueueADT create() {
        return new ArrayQueueADT();
    }

    // Pre: element != null
    // Post: n' = n + 1 && immutable(n) && a'[n'] = element
    public static void enqueue(ArrayQueueADT queue, Object element) {
        assert element != null;
        ensureCapacity(queue);
        queue.size++;
        queue.last = modSum(queue, queue.last, 1);
        queue.elements[queue.last] = element;
        if (queue.size == 1) {
            queue.first = queue.last;
        }
    }

    // Pre: n > 0
    // Post: return a[1] && n' = n && immutable(n)
    public static Object element(ArrayQueueADT queue) {
        assert queue.size > 0;
        return queue.elements[queue.first];
    }

    // Pre: n > 0
    // Post: return a[1] && n' = n - 1 && forall 1 <= i < n: a'[i] = a[i + 1]
    public static Object dequeue(ArrayQueueADT queue) {
        assert queue.size > 0;
        queue.size--;
        Object value = queue.elements[queue.first];
        queue.elements[queue.first] = null;
        queue.first = modSum(queue, queue.first, 1);
        return value;
    }

    // Post: return n && immutable(n) && n' = n
    public static int size(ArrayQueueADT queue) {
        return queue.size;
    }

    // Post: return n == 0 && immutable(n) && n' = n
    public static boolean isEmpty(ArrayQueueADT queue) {
        return queue.size == 0;
    }

    // Post: n' = 0 && a' = {}
    public static void clear(ArrayQueueADT queue) {
        queue.size = 0;
        queue.first = 0;
        queue.last = 0;
        queue.elements = new Object[5];
    }

    private static void ensureCapacity(ArrayQueueADT queue) {
        if (queue.size < queue.elements.length) {
            return;
        }

        Object[] newElements = new Object[queue.elements.length * 2];
        for (int i = 0, j = queue.first; i < queue.size; i++, j++) {
            if (j == queue.elements.length) {
                j = 0;
            }
            newElements[i] = queue.elements[j];
        }
        queue.first = 0;
        queue.last = queue.elements.length - 1;
        queue.elements = newElements;
    }

    private static int modSum(ArrayQueueADT queue, int first, int second) {
        first += second;
        if (first >= queue.elements.length) {
            return first - queue.elements.length;
        } else {
            return first;
        }
    }

    private static int modSub(ArrayQueueADT queue, int first, int second) {
        first -= second;
        if (first < 0) {
            return queue.elements.length + first;
        } else {
            return first;
        }
    }
}
