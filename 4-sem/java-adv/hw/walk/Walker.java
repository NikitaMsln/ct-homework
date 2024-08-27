package walk;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.InvalidPathException;
import java.nio.file.Path;
import java.nio.file.attribute.BasicFileAttributes;

import static java.nio.file.Files.walkFileTree;

public class Walker {
    private final Logger logger;
    private final boolean recursive;

    public Walker(PrintStream errorStream, boolean isRecursive) {
        logger = new Logger(errorStream);
        recursive = isRecursive;
    }

    private static Path createFile(Path path) throws IOException {
        if (Files.exists(path)) {
            return path;
        }
        Path parent = path.getParent();
        if (parent != null && Files.notExists(parent)) {
            Files.createDirectories(parent);
        }
        return Files.createFile(path);
    }

    public void walk(String inFile, String outFile, HashEvaluator<InputStream> evaluator) {
        try (
                Reader in = new FileReader(inFile, StandardCharsets.UTF_8);
                Writer out = new FileWriter(createFile(Path.of(outFile)).toString(), StandardCharsets.UTF_8)
        ) {
            walk(in, out, evaluator);
        } catch (IOException | SecurityException | InvalidPathException  exception) {
            logger.log(exception.getMessage());
        }
    }

    public void walk(Reader in, Writer out, HashEvaluator<InputStream> evaluator) throws IOException {
        LineNumberReader lineReader = new LineNumberReader(in);
        HashFileVisitor visitor = new HashFileVisitor(evaluator, out, logger);

        while (true) {
            String filename;
            try {
                filename = lineReader.readLine();
                if (filename == null) {
                    break;
                }
            } catch (IOException e) {
                logger.log(e.getMessage());
                break;
            }
            try {
                Path filePath = Path.of(filename);
                if (Files.isDirectory(filePath)) {
                    if (recursive) {
                        walkFileTree(filePath, visitor);
                    } else {
                        logger.log(filename + " is directory");
                        out.write(evaluator.formattedHash(null) + " " + filename + System.lineSeparator());
                    }
                } else if (Files.exists(filePath)) {
                    visitor.visitFile(filePath, Files.readAttributes(filePath, BasicFileAttributes.class));
                } else {
                    logger.log("File " + filename + " doesn't exist");
                    out.write(evaluator.formattedHash(null) + " " + filename + System.lineSeparator());
                }
            } catch (IOException | SecurityException | InvalidPathException exception) {
                logger.log(exception.getMessage());
                out.write(evaluator.formattedHash(null) + " " + filename + System.lineSeparator());
            }
        }
    }
}

