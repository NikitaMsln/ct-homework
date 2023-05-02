package queue;

public class MyQueuesTest {
    public static void main(String[] args) {
        try {
            System.out.println("Testing ArrayQueue:");
            System.out.println((MyArrayQueueTest.test()) ? "Tests completed" : "Tests failed");
            System.out.println("Testing ArrayQueueADT:");
            System.out.println((MyArrayQueueADTTest.test()) ? "Tests completed" : "Tests failed");
            System.out.println("Testing ArrayQueueModule:");
            System.out.println((MyArrayQueueModuleTest.test()) ? "Tests completed" : "Tests failed");
        } catch (Exception e) {
            System.out.println("Tests failed: " + e.getMessage());
        }
    }
}
