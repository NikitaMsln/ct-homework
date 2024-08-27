package walk;

import java.io.IOException;
import java.io.InputStream;
import java.io.Writer;
import java.nio.file.FileVisitResult;
import java.nio.file.FileVisitor;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.BasicFileAttributes;

public class HashFileVisitor implements FileVisitor<Path> {
    private final HashEvaluator<InputStream> evaluator;
    private final Writer resultOut;
    private final Logger logger;

    public HashFileVisitor(HashEvaluator<InputStream> hashEvaluator, Writer out, Logger logs) {
        assert (hashEvaluator != null && out != null);
        evaluator = hashEvaluator;
        resultOut = out;
        logger = logs;
    }

    @Override
    public FileVisitResult preVisitDirectory(Path dir, BasicFileAttributes attrs) {
        return FileVisitResult.CONTINUE;
    }

    @Override
    public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) throws IOException {
        String hash;
        try (
                InputStream fileInput = Files.newInputStream(file)
        ) {
            hash = evaluator.formattedHash(fileInput);
        } catch (IOException | SecurityException exception) {
            logger.log(exception.getMessage());
            hash = evaluator.formattedHash(null);
        }
        resultOut.write(hash + " " + file + System.lineSeparator());
        return FileVisitResult.CONTINUE;
    }

    @Override
    public FileVisitResult visitFileFailed(Path file, IOException exc) {
        return FileVisitResult.CONTINUE;
    }

    @Override
    public FileVisitResult postVisitDirectory(Path dir, IOException exc) {
        return FileVisitResult.CONTINUE;
    }
}

