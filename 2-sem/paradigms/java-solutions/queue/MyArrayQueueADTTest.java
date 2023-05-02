package queue;

public class MyArrayQueueADTTest {
    public static boolean test() {
        ArrayQueueADT queue = ArrayQueueADT.create();
        String prefix = "element ";

        int actal_size = 0;

        for (int i = 1; i <= 1000; i++) {
            ArrayQueueADT.enqueue(queue, prefix + i);
            actal_size++;
            if (ArrayQueueADT.size(queue) != actal_size) {
                System.out.println("The contract is not completed: size() return wring size or enqueue() didn't add 1 element");
                return false;
            }
        }

        for (int i = 1; i <= 500; i++) {
            if (!ArrayQueueADT.element(queue).equals(prefix + i)) {
                System.out.println("The contract is not completed: element() return wrong element");
                return false;
            }
            if (!ArrayQueueADT.dequeue(queue).equals(prefix + i)) {
                System.out.println("The contract is not completed: dequeue() return wrong element");
                return false;
            }
            actal_size--;
            if (ArrayQueueADT.size(queue) != actal_size) {
                System.out.println("The contract is not completed: size() return wring size or dequeue() didn't remove 1 element");
                return false;
            }
        }

        for (int i = 1; i <= 1000; i++) {
            ArrayQueueADT.push(queue, prefix + i);
            actal_size++;
            if (ArrayQueueADT.size(queue) != actal_size) {
                System.out.println("The contract is not completed: size() return wring size or push() didn't add 1 element");
                return false;
            }
        }

        for (int i = 1, j = 1000; i <= 500; i++, j--) {
            if (!ArrayQueueADT.peek(queue).equals(prefix + j)) {
                System.out.println("The contract is not completed: peek() return wrong element");
                return false;
            }
            if (!ArrayQueueADT.remove(queue).equals(prefix + j)) {
                System.out.println("The contract is not completed: remove() return wrong element");
                return false;
            }
            actal_size--;
            if (ArrayQueueADT.size(queue) != actal_size) {
                System.out.println("The contract is not completed: size() return wring size or remove() didn't remove 1 element");
                return false;
            }
        }

        for (int i = actal_size; i >= 0; i--) {
            ArrayQueueADT.set(queue, i, prefix + "changed " + i);
            if (ArrayQueueADT.size(queue) != actal_size) {
                System.out.println("The contract is not completed: size() return wring size or set() change size of queue");
                return false;
            }
        }

        for (int i = actal_size; i >= 0; i--) {
            if (!ArrayQueueADT.get(queue, i).equals(prefix + "changed " + i)) {
                System.out.println("The contract is not completed: get() return wrong value of set() change wrong element");
                return false;
            }
            if (ArrayQueueADT.size(queue) != actal_size) {
                System.out.println("The contract is not completed: size() return wring size or get() change size of queue");
                return false;
            }
        }

        if (ArrayQueueADT.isEmpty(queue)) {
            System.out.println("The contract is not completed: isEmpty() return wrong answer");
            return false;
        }

        ArrayQueueADT.clear(queue);
        if (!ArrayQueueADT.isEmpty(queue)) {
            System.out.println("The contract is not completed: isEmpty() return wrong answer or clear() didn't clear queue");
            return false;
        } else if (ArrayQueueADT.size(queue) != 0) {
            System.out.println("The contract is not completed: size() return wrong answer");
            return false;
        }

        ArrayQueueADT.clear(queue);
        if (!ArrayQueueADT.isEmpty(queue)) {
            System.out.println("The contract is not completed: isEmpty() return wrong answer or clear() didn't clear queue");
            return false;
        } else if (ArrayQueueADT.size(queue) != 0) {
            System.out.println("The contract is not completed: size() return wrong answer");
            return false;
        }

        return true;
    }
}
