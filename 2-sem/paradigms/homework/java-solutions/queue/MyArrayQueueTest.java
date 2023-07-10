package queue;

public class MyArrayQueueTest {
    public static boolean test() {
        ArrayQueue queue = new ArrayQueue();

        if(!MyQueueInterfaceTest.test(queue, 500)) {
            return false;
        }

        queue.clear();
        if (!queue.isEmpty()) {
            System.out.println("The contract is not completed: isEmpty() return wrong answer or clear() didn't clear queue");
            return false;
        } else if (queue.size() != 0) {
            System.out.println("The contract is not completed: size() return wrong answer");
            return false;
        }

        int size = 1000;

        for (int i = 0; i < size; i++) {
            queue.enqueue("element" + i);
        }

        for (int i = size; i >= 0; i--) {
            queue.set(i, "changed " + i);
            if (queue.size() != size) {
                System.out.println("The contract is not completed: size() return wring size or set() change size of queue");
                return false;
            }
        }

        for (int i = size; i >= 0; i--) {
            if (!queue.get(i).equals("changed " + i)) {
                System.out.println("The contract is not completed: get() return wrong value of set() change wrong element");
                return false;
            }
            if (queue.size() != size) {
                System.out.println("The contract is not completed: size() return wring size or get() change size of queue");
                return false;
            }
        }

        return true;
    }
}
