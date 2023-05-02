package queue;

public class ArrayQueue extends AbstractQueue {
    private int first = 0;
    private int last = 0;
    private Object[] elements = new Object[5];

    public void set(int index, Object element) {
        assert index >= 0 && index < size && element != null;
        index = modSum(index, first);
        elements[index] = element;
    }

    public Object get(int index) {
        assert index >= 0 && index < size;
        index = modSum(index, first);
        return elements[index];
    }

    @Override
    protected Queue generate() {
        return new ArrayQueue();
    }

    @Override
    protected void pushImpl(Object element) {
        ensureCapacity();
        first = modSub(first, 1);
        elements[first] = element;
        if (size == 0) {
            last = first;
        }
    }

    @Override
    protected Object peekImpl() {
        return elements[last];
    }

    @Override
    protected void removeTail() {
        elements[last] = null;
        last = modSub(last, 1);
    }

    @Override
    protected void enqueueImpl(Object element) {
        ensureCapacity();
        last = modSum(last, 1);
        elements[last] = element;
        if (size == 0) {
            first = last;
        }
    }

    @Override
    protected Object elementImpl() {
        return elements[first];
    }

    @Override
    protected void removeHead() {
        elements[first] = null;
        first = modSum(first, 1);
    }

    @Override
    public void clearImpl() {
        first = 0;
        last = 0;
        elements = new Object[5];
    }

    private void ensureCapacity() {
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

    private int modSum(int first, int second) {
        first += second;
        if (first >= elements.length) {
            return first - elements.length;
        } else {
            return first;
        }
    }

    private int modSub(int first, int second) {
        first -= second;
        if (first < 0) {
            return elements.length + first;
        } else {
            return first;
        }
    }
}
