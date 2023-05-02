package queue;

public abstract class AbstractQueue implements Queue {
    protected int size = 0;

    @Override
    public Queue getNth(int k) {
        Queue result = generate();
        for (int i = 1, multiK = k; i <= size; i++) {
            Object value = dequeue();
            enqueue(value);
            if (i == multiK) {
                multiK += k;
                result.enqueue(value);
            }
        }
        return result;
    }

    protected abstract Queue generate();

    @Override
    public void dropNth(int k) {
        int oldSize = size;
        for (int i = 1, multiK = k; i <= oldSize; i++) {
            Object value = dequeue();
            if (i == multiK) {
                multiK += k;
            } else {
                enqueue(value);
            }
        }
    }

    @Override
    public Queue removeNth(int k) {
        Queue result = getNth(k);
        dropNth(k);
        return result;
    }

    @Override
    public void push(Object element) {
        assert element != null;
        pushImpl(element);
        size++;
    }

    protected abstract void pushImpl(Object element);

    @Override
    public Object peek() {
        assert size > 0;
        return peekImpl();
    }

    protected abstract Object peekImpl();

    @Override
    public Object remove() {
        assert size > 0;
        Object result =  peek();
        size--;
        removeTail();
        return result;
    }

    protected abstract void removeTail();

    @Override
    public void enqueue(Object element) {
        assert element != null;
        enqueueImpl(element);
        size++;
    }

    protected abstract void enqueueImpl(Object element);

    @Override
    public Object element() {
        assert size > 0;
        return elementImpl();
    }

    protected abstract Object elementImpl();

    @Override
    public Object dequeue() {
        assert size > 0;
        Object result = elementImpl();
        size--;
        removeHead();
        return result;
    }

    protected abstract void removeHead();

    @Override
    public void clear() {
        size = 0;
        clearImpl();
    }

    protected abstract void clearImpl();

    @Override
    public int size() {
        return size;
    }

    @Override
    public boolean isEmpty() {
        return size == 0;
    }
}
