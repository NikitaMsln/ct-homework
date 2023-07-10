package queue;

/*
Model: a[1]...a[n]
Invariant: forall 1 <= i <= n: a[i] != null
Let immutable(n): n >= 0 && for i=1...n: a'[i] = a[i]
*/
public interface Queue {
    // Post: return queue b: forall 0 <= i <= n / k: b[i] = a[i * k] && immutable(n) && n' = n
    Queue getNth(int k);

    // Post: return queue b: b[i] = a[i * k] && forall 0 <= i <= n / k: forall 1 <= j <= k - 1: a'[i * (k - 1) + j] = a[i * k + j]
    Queue removeNth(int k);

    // Post: forall 0 <= i <= n / k: forall 1 <= j <= k - 1: a'[i * (k - 1) + j] = a[i * k + j]
    void dropNth(int k);

    // Pre: element != null
    // Post: n' = n + 1 && n >= 0 && for i=2...n: a'[i-1] = a[i] && a'[1] = element
    void push(Object element);

    // Pre: n > 0
    // Post: return a[n] && immutable(n) && n' = n
    Object peek();

    // Pre: n > 0
    // Post: return a[1] && n' = n - 1 && immutable(n)
    Object remove();

    // Pre: element != null
    // Post: n' = n + 1 && immutable(n) && a'[n'] = element
    void enqueue(Object element);

    // Pre: n > 0
    // Post: return a[1] && n' = n && immutable(n)
    Object element();

    // Pre: n > 0
    // Post: return a[1] && n' = n - 1 && forall 1 <= i < n: a'[i] = a[i + 1]
    Object dequeue();

    // Post: return n && immutable(n) && n' = n
    int size();

    // Post: return n == 0 && immutable(n) && n' = n
    boolean isEmpty();

    // Post: n' = 0 && a' = {}
    void clear();
}
