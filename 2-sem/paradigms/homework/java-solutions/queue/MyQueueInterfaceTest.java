package queue;

public class MyQueueInterfaceTest {
    public static void main(String[] args) {
        try {
            System.out.println("Testing ArrayQueue:");
            System.out.println((test(new ArrayQueue(), 1000)) ? "Tests completed" : "Tests failed");
            System.out.println("Testing LinkedQueue:");
            System.out.println((test(new LinkedQueue(), 1000)) ? "Tests completed" : "Tests failed");
        } catch (Exception e) {
            System.out.println("Tests failed: " + e.getMessage());
        }
    }

    public static boolean test(Queue queue, int repet) {
        String prefix = "element ";

        int actal_size = 0;

        queue.clear();
        if (!queue.isEmpty()) {
            System.out.println("The contract is not completed: isEmpty() return wrong answer or clear() didn't clear queue");
            return false;
        } else if (queue.size() != 0) {
            System.out.println("The contract is not completed: size() return wrong answer");
            return false;
        }

        for (int i = 1; i <= 4 * repet; i++) {
            queue.enqueue(prefix + ((i + 1) / 2));
            actal_size++;
            if (queue.size() != actal_size) {
                System.out.println("The contract is not completed: size() return wring size or enqueue() didn't add 1 element");
                return false;
            }
        }

        Queue gottenQueue = queue.getNth(-1);
        if (gottenQueue.size() != 0 || !gottenQueue.isEmpty()) {
            System.out.println("The contract is not completed: getNth() return wrong queue");
            return false;
        }

        gottenQueue = queue.removeNth(-1);
        if (gottenQueue.size() != 0 || !gottenQueue.isEmpty()) {
            System.out.println("The contract is not completed: removeNth() return wrong queue");
            return false;
        }

        gottenQueue = queue.getNth(actal_size + 1);
        if (gottenQueue.size() != 0 || !gottenQueue.isEmpty()) {
            System.out.println("The contract is not completed: getNth() return wrong queue");
            return false;
        }

        gottenQueue = queue.removeNth(actal_size + 1);
        if (gottenQueue.size() != 0 || !gottenQueue.isEmpty()) {
            System.out.println("The contract is not completed: removeNth() return wrong queue");
            return false;
        }

        gottenQueue = queue.getNth(1);
        if (gottenQueue.size() != actal_size) {
            System.out.println("The contract is not completed: getNth() return wrong queue");
            return false;
        }

        for (int i = 1; i <= 4 * repet; i++) {
            Object value = gottenQueue.dequeue();
            if (!value.equals(prefix + ((i + 1) / 2))) {
                System.out.println("The contract is not completed: getNth() return wrong queue");
                return false;
            }
            gottenQueue.enqueue(value);
        }

        gottenQueue = queue.removeNth(2);
        actal_size /= 2;
        if (gottenQueue.size() != actal_size) {
            System.out.println("The contract is not completed: removeNth() return wrong queue");
            return false;
        }
        if (queue.size() != actal_size) {
            System.out.println("The contract is not completed: removeNth() remove incorrect values");
            return false;
        }

        for (int i = 1; i <= 2 * repet; i++) {
            Object value = gottenQueue.dequeue();
            if (!value.equals(prefix + i)) {
                System.out.println("The contract is not completed: removeNth() return wrong queue");
                return false;
            }
            gottenQueue.enqueue(value);
            value = queue.dequeue();
            if (!value.equals(prefix + i)) {
                System.out.println("The contract is not completed: removeNth() remove wrong value");
                return false;
            }
            queue.enqueue(value);
        }

        gottenQueue.dropNth(-1);
        if (gottenQueue.size() != actal_size) {
            System.out.println("The contract is not completed: dropNth() remove wrong value");
            return false;
        }

        gottenQueue.dropNth(actal_size + 1);
        if (gottenQueue.size() != actal_size) {
            System.out.println("The contract is not completed: dropNth() remove wrong value");
            return false;
        }


        gottenQueue.dropNth(2);
        if (gottenQueue.size() != actal_size / 2) {
            System.out.println("The contract is not completed: dropNth() remove wrong value");
            return false;
        }

        for (int i = 1; i < 2 * repet; i++) {
            if (i % 2 != 0) {
                if (!gottenQueue.dequeue().equals(prefix + i)) {
                    System.out.println("The contract is not completed: dropNth() remove wrong value");
                    return false;
                }
            }
        }

        for (int i = 1; i <= 2 * repet; i++) {
            queue.enqueue(prefix + i);
            actal_size++;
            if (queue.size() != actal_size) {
                System.out.println("The contract is not completed: size() return wring size or enqueue() didn't add 1 element");
                return false;
            }
        }

        for (int i = 1; i <= repet; i++) {
            if (!queue.element().equals(prefix + i)) {
                System.out.println("The contract is not completed: element() return wrong element");
                return false;
            }
            if (!queue.dequeue().equals(prefix + i)) {
                System.out.println("The contract is not completed: dequeue() return wrong element");
                return false;
            }
            actal_size--;
            if (queue.size() != actal_size) {
                System.out.println("The contract is not completed: size() return wring size or dequeue() didn't remove 1 element");
                return false;
            }
        }

        for (int i = 1; i <= 2 * repet; i++) {
            queue.push(prefix + i);
            actal_size++;
            if (queue.size() != actal_size) {
                System.out.println("The contract is not completed: size() return wring size or push() didn't add 1 element");
                return false;
            }
        }

        for (int i = 1, j = 2 * repet; i <= repet; i++, j--) {
            if (!queue.peek().equals(prefix + j)) {
                System.out.println("The contract is not completed: peek() return wrong element");
                return false;
            }
            if (!queue.remove().equals(prefix + j)) {
                System.out.println("The contract is not completed: remove() return wrong element");
                return false;
            }
            actal_size--;
            if (queue.size() != actal_size) {
                System.out.println("The contract is not completed: size() return wring size or remove() didn't remove 1 element");
                return false;
            }
        }

        if (queue.isEmpty()) {
            System.out.println("The contract is not completed: isEmpty() return wrong answer");
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

        queue.clear();
        if (!queue.isEmpty()) {
            System.out.println("The contract is not completed: isEmpty() return wrong answer or clear() didn't clear queue");
            return false;
        } else if (queue.size() != 0) {
            System.out.println("The contract is not completed: size() return wrong answer");
            return false;
        }

        return true;
    }
}
