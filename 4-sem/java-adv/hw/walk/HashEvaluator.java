package walk;

public interface HashEvaluator<T> {
    byte[] hash(T value);

    String formattedHash(T value);
}

