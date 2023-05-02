package queue;

public class LinkedQueue extends AbstractQueue {
    private Node head = null;
    private Node tail = null;

    @Override
    protected Queue generate() {
        return new LinkedQueue();
    }

    @Override
    protected void pushImpl(Object element) {
        head = new Node(element, null, head);
        if (head.next != null) {
            head.next.prev = head;
        }
        if (size == 0) {
            tail = head;
        }
    }

    @Override
    protected Object peekImpl() {
        return tail.value;
    }

    @Override
    protected void removeTail() {
        tail = tail.prev;
    }

    @Override
    protected void enqueueImpl(Object element) {
        tail = new Node(element, tail, null);
        if (tail.prev != null) {
            tail.prev.next = tail;
        }
        if (size == 0) {
            head = tail;
        }
    }

    @Override
    protected Object elementImpl() {
        return head.value;
    }

    @Override
    protected void removeHead() {
        head = head.next;
    }

    @Override
    protected void clearImpl() {
        head = null;
        tail = null;
    }

    private static class Node {
        private final Object value;
        private Node next;
        private Node prev;

        public Node(Object value, Node prev, Node next) {
            this.value = value;
            this.prev = prev;
            this.next = next;
        }
    }
}
