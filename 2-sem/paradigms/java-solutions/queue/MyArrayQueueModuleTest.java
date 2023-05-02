package queue;

public class MyArrayQueueModuleTest {
    public static boolean test() {
        String prefix = "element ";

        ArrayQueueModule.clear();
        if (!ArrayQueueModule.isEmpty()) {
            System.out.println("The contract is not completed: isEmpty() return wrong answer or clear() didn't clear queue");
            return false;
        } else if (ArrayQueueModule.size() != 0) {
            System.out.println("The contract is not completed: size() return wrong answer");
            return false;
        }

        ArrayQueueModule.clear();
        if (!ArrayQueueModule.isEmpty()) {
            System.out.println("The contract is not completed: isEmpty() return wrong answer or clear() didn't clear queue");
            return false;
        } else if (ArrayQueueModule.size() != 0) {
            System.out.println("The contract is not completed: size() return wrong answer");
            return false;
        }

        int actal_size = 0;

        for (int i = 1; i <= 1000; i++) {
            ArrayQueueModule.enqueue(prefix + i);
            actal_size++;
            if (ArrayQueueModule.size() != actal_size) {
                System.out.println("The contract is not completed: size() return wring size or enqueue() didn't add 1 element");
                return false;
            }
        }

        for (int i = 1; i <= 500; i++) {
            if (!ArrayQueueModule.element().equals(prefix + i)) {
                System.out.println("The contract is not completed: element() return wrong element");
                return false;
            }
            if (!ArrayQueueModule.dequeue().equals(prefix + i)) {
                System.out.println("The contract is not completed: dequeue() return wrong element");
                return false;
            }
            actal_size--;
            if (ArrayQueueModule.size() != actal_size) {
                System.out.println("The contract is not completed: size() return wring size or dequeue() didn't remove 1 element");
                return false;
            }
        }

        for (int i = 1; i <= 1000; i++) {
            ArrayQueueModule.push(prefix + i);
            actal_size++;
            if (ArrayQueueModule.size() != actal_size) {
                System.out.println("The contract is not completed: size() return wring size or push() didn't add 1 element");
                return false;
            }
        }

        for (int i = 1, j = 1000; i <= 500; i++, j--) {
            if (!ArrayQueueModule.peek().equals(prefix + j)) {
                System.out.println("The contract is not completed: peek() return wrong element");
                return false;
            }
            if (!ArrayQueueModule.remove().equals(prefix + j)) {
                System.out.println("The contract is not completed: remove() return wrong element");
                return false;
            }
            actal_size--;
            if (ArrayQueueModule.size() != actal_size) {
                System.out.println("The contract is not completed: size() return wring size or remove() didn't remove 1 element");
                return false;
            }
        }

        for (int i = actal_size; i >= 0; i--) {
            ArrayQueueModule.set(i, prefix + "changed " + i);
            if (ArrayQueueModule.size() != actal_size) {
                System.out.println("The contract is not completed: size() return wring size or set() change size of queue");
                return false;
            }
        }

        for (int i = actal_size; i >= 0; i--) {
            if (!ArrayQueueModule.get(i).equals(prefix + "changed " + i)) {
                System.out.println("The contract is not completed: get() return wrong value of set() change wrong element");
                return false;
            }
            if (ArrayQueueModule.size() != actal_size) {
                System.out.println("The contract is not completed: size() return wring size or get() change size of queue");
                return false;
            }
        }

        if (ArrayQueueModule.isEmpty()) {
            System.out.println("The contract is not completed: isEmpty() return wrong answer");
            return false;
        }

        return true;
    }
}
