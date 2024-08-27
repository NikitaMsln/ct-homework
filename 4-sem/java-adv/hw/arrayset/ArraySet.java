package arrayset;

import java.util.*;
import java.util.function.UnaryOperator;

public class ArraySet<E> extends AbstractSet<E> implements NavigableSet<E>, List<E> {
    private final List<E> data;
    private final Comparator<? super E> comp;

    public ArraySet() {
        this(List.of(), null);
    }

    public ArraySet(Comparator<? super E> comparator) {
        this(List.of(), comparator);
    }

    public ArraySet(Collection<? extends E> collection) {
        this(collection, null);
    }

    public ArraySet(Collection<? extends E> collection, Comparator<? super E> comparator) {
        comp = comparator;
        Set<E> values = new TreeSet<>(comp);
        values.addAll(collection);
        data = values.stream().toList();
    }

    private ArraySet(boolean isSorted, List<E> list, Comparator<? super E> comparator) {
        assert(isSorted);
        data = list;
        comp = comparator;
    }

    @Override
    public E lower(E e) {
        return getOrNull(find(e, -1, -1));
    }

    @Override
    public E floor(E e) {
        return getOrNull(find(e, 0, -1));
    }

    @Override
    public E ceiling(E e) {
        return getOrNull(find(e, 0, 0));
    }

    @Override
    public E higher(E e) {
        return getOrNull(find(e, 1, 0));
    }

    @Override
    public boolean contains(Object o) {
        return indexOf(o) >= 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int indexOf(Object o) {
        int index = find((E) o, 0, data.size());
        return (index >= 0 && index < data.size()) ? index : -1;
    }

    private int find(E e, int equalAdd, int moreAdd) {
        int index = Collections.binarySearch(data, e, comp);
        if (index < 0) {
            return -(index + 1) + moreAdd;
        } else {
            return index + equalAdd;
        }
    }

    private E getOrNull(int index) {
        return (index >= 0 && index < data.size()) ? data.get(index) : null;
    }

    @Override
    public E pollFirst() throws UnsupportedOperationException {
        throw new UnsupportedOperationException("ArraySet is immutable: ArraySet.pollFirst is unsupported");
    }

    @Override
    public E pollLast() throws UnsupportedOperationException {
        throw new UnsupportedOperationException("ArraySet is immutable: ArraySet.pollLast is unsupported");
    }

    @Override
    public Iterator<E> iterator() {
        return data.iterator();
    }

    @Override
    public Spliterator<E> spliterator() {
        return super.spliterator();
    }

    @Override
    public NavigableSet<E> descendingSet() {
        return reversed();
    }

    @Override
    public ArraySet<E> reversed() {
        return new ArraySet<>(true, data.reversed(), Collections.reverseOrder(comp));
    }

    @Override
    public Iterator<E> descendingIterator() {
        return reversed().iterator();
    }

    @Override
    public Comparator<? super E> comparator() {
        return comp;
    }

    @Override
    public NavigableSet<E> subSet(E fromElement, boolean fromInclusive, E toElement, boolean toInclusive) {
        if (Collections.reverseOrder(comp).reversed().compare(fromElement, toElement) > 0) {
            throw new IllegalArgumentException("Illegal arguments: fromElement is greater than toElement in ArraySet.subSet");
        }
        return subSetImplementation(fromElement, fromInclusive, toElement, toInclusive);
    }

    @Override
    public NavigableSet<E> headSet(E toElement, boolean inclusive) {
        if (isEmpty()) {
            return new ArraySet<>(comp);
        }
        return subSetImplementation(first(), true, toElement, inclusive);
    }

    @Override
    public NavigableSet<E> tailSet(E fromElement, boolean inclusive) {
        if (isEmpty()) {
            return new ArraySet<>(comp);
        }
        return subSetImplementation(fromElement, inclusive, last(), true);
    }

    @Override
    public SortedSet<E> subSet(E fromElement, E toElement) {
        return subSet(fromElement, true, toElement, false);
    }

    @Override
    public SortedSet<E> headSet(E toElement) {
        return headSet(toElement, false);
    }

    @Override
    public SortedSet<E> tailSet(E fromElement) {
        return tailSet(fromElement, true);
    }

    private ArraySet<E> subSetImplementation(E fromElement, boolean fromInclusive, E toElement, boolean toInclusive) {
        int indexFrom = find(fromElement, fromInclusive ? 0 : 1, 0);
        int indexTo = find(toElement, toInclusive ? 1 : 0, 0);
        if (indexFrom > indexTo) {
            return new ArraySet<>(comparator());
        }
        return new ArraySet<>(true, data.subList(indexFrom, indexTo), comp);
    }

    @Override
    public E first() {
        return data.getFirst();
    }

    @Override
    public E last() {
        return data.getLast();
    }

    @Override
    public E getFirst() {
        return data.getFirst();
    }

    @Override
    public E getLast() {
        return data.getLast();
    }

    @Override
    public void addFirst(E e) throws UnsupportedOperationException {
        throw new UnsupportedOperationException("ArraySet is immutable: ArraySet.addFirst is unsupported");
    }

    @Override
    public void addLast(E e) throws UnsupportedOperationException {
        throw new UnsupportedOperationException("ArraySet is immutable: ArraySet.addLast is unsupported");
    }

    @Override
    public E removeFirst() throws UnsupportedOperationException {
        throw new UnsupportedOperationException("ArraySet is immutable: ArraySet.removeFirst is unsupported");
    }

    @Override
    public E removeLast() throws UnsupportedOperationException {
        throw new UnsupportedOperationException("ArraySet is immutable: ArraySet.removeLast is unsupported");
    }

    @Override
    public boolean addAll(int index, Collection<? extends E> c) throws UnsupportedOperationException {
        throw new UnsupportedOperationException("ArraySet is immutable: ArraySet.addAll is unsupported");
    }

    @Override
    public void replaceAll(UnaryOperator<E> operator) throws UnsupportedOperationException {
        throw new UnsupportedOperationException("ArraySet is immutable: ArraySet.replaceAll is unsupported");
    }

    @Override
    public E get(int index) {
        return data.get(index);
    }

    @Override
    public E set(int index, E element) throws UnsupportedOperationException {
        throw new UnsupportedOperationException("ArraySet is immutable: ArraySet.set is unsupported");
    }

    @Override
    public void add(int index, E element) throws UnsupportedOperationException {
        throw new UnsupportedOperationException("ArraySet is immutable: ArraySet.add is unsupported");
    }

    @Override
    public E remove(int index) throws UnsupportedOperationException {
        throw new UnsupportedOperationException("ArraySet is immutable: ArraySet.remove is unsupported");
    }

    @Override
    public int lastIndexOf(Object o) {
        return indexOf(o);
    }

    @Override
    public ListIterator<E> listIterator() {
        return data.listIterator();
    }

    @Override
    public ListIterator<E> listIterator(int index) {
        return data.listIterator(index);
    }

    @Override
    public List<E> subList(int fromIndex, int toIndex) {
        return new ArraySet<>(true, data.subList(fromIndex, toIndex), comp);
    }

    @Override
    public int size() {
        return data.size();
    }
}

