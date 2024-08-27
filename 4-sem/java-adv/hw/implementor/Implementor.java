package implementor;

import javax.tools.JavaCompiler;
import javax.tools.ToolProvider;
import java.io.*;
import java.lang.reflect.*;
import java.net.URISyntaxException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;
import java.util.function.Consumer;
import java.util.jar.JarOutputStream;
import java.util.jar.Manifest;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.util.zip.ZipEntry;

public class Implementor implements JarImpler {
    /**
     * Create a path to file to class with extension {@code String ext} by his package and root folder
     *
     * @param token class to which the path will create
     * @param root dir with package
     * @param ext extension of file
     * @return path to that file
     */
    private static Path packageToPath(Class<?> token, Path root, String ext) {
        return Path.of(
                root.toString().replace(root.getFileSystem().getSeparator(), root.getFileSystem().getSeparator())
                        + root.getFileSystem().getSeparator()
                        + token.getPackageName().replace(".", root.getFileSystem().getSeparator())
                        + root.getFileSystem().getSeparator()
                        + token.getSimpleName()
                        + "Impl."
                        + ext
        );
    }

    /**
     * Create the file by path if it hasn't created yet or clear already created file
     *
     * @param path path to file
     * @return path to this file
     * @throws IOException if the file can't to be created or cleared
     * @throws SecurityException if it has not roots to create or clear file
     */
    private static Path createFile(Path path) throws IOException, SecurityException {
        Files.createDirectories(path.getParent());
        if (Files.exists(path)) {
            Files.delete(path);
        }
        return Files.createFile(path);
    }

    /**
     * Visit all ancestors of the class except {@link java.lang.Object} and applies function
     *
     * @param token start class
     * @param consumer function applied to the class and his ancestors
     */
    private static void visitParents(Class<?> token, Consumer<Class<?>> consumer) {
        while (token != null) {
            consumer.accept(token);
            token = token.getSuperclass();
        }
    }

    /**
     * Check if first extends second
     *
     * @param first checked to child
     * @param second checked to ancestor
     * @return true if first extends second
     */
    private static boolean isChild(Class<?> first, Class<?> second) {
        final boolean[] result = {false};
        visitParents(first, (clazz) -> result[0] = result[0] || clazz == second);
        return second == Object.class || result[0];
    }

    /**
     * Choose method with more specified return type
     * <p>
     * If current method is final return his
     * <p>
     * If one of methods is null or return types isn't comparable
     * return null
     * <p>
     * If methods have same return type return current method
     *
     * @param currentMethod updated method
     * @param newMethod new variant of overloading
     * @return method more specified return type
     */
    private static Method chooseMethod(Method currentMethod, Method newMethod) {
        if (currentMethod == null || Modifier.isFinal(currentMethod.getModifiers())) {
            return currentMethod;
        } else if (newMethod == null) {
            return null;
        }
        if (isChild(currentMethod.getReturnType(), newMethod.getReturnType())) {
            return currentMethod;
        } else if (isChild(newMethod.getReturnType(), currentMethod.getReturnType())) {
            return newMethod;
        } else {
            return null;
        }
    }

    /**
     * Return abstract method overload mapping to one of the corresponding ones with most specified return type
     *
     * @param token class whose methods are taken
     * @return map a overload variant to a method
     * @throws ImplerException if exist many methods of the same overload variant with incomparable return types
     */
    private static Map<String, Method> getMethods(Class<?> token) throws ImplerException {
        Map<String, Method> result = new HashMap<>();
        visitParents(token, (clazz) ->
                Stream.concat(Arrays.stream(clazz.getDeclaredMethods()), Arrays.stream(clazz.getMethods()))
                        .forEach(method -> result.merge(
                                method.getName()
                                        + "("
                                        + Arrays.stream(method.getParameters())
                                        .map(
                                                param -> getName(param.getType())
                                                        + " "
                                                        + param.getName()
                                        )
                                        .collect(Collectors.joining(", "))
                                        + ")",
                                method,
                                Implementor::chooseMethod)
                        )
        );
        if (result.containsValue(null)) {
            throw new ImplerException("Cannot create a method of class");
        }
        return result.entrySet().stream()
                .filter(entry -> Modifier.isAbstract(entry.getValue().getModifiers()))
                .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));
    }

    /**
     * Return a mapping od constructor overloads to the first in the ancestors
     *
     * @param token class whose constructors are taken
     * @return map a overload variant to a constructor
     */
    private static Map<String, Constructor<?>> getConstructors(Class<?> token) {
        return Stream.concat(Arrays.stream(token.getConstructors()), Arrays.stream(token.getDeclaredConstructors()))
                .filter(constructor -> !Modifier.isPrivate(constructor.getModifiers()))
                .collect(
                        HashMap::new,
                        (Map<String, Constructor<?>> map, Constructor<?> constructor) -> map.merge(
                                Arrays.stream(constructor.getParameters())
                                        .map(param -> getName(param.getType()) + " " + param.getName())
                                        .collect(Collectors.joining(", ")),
                                constructor,
                                (a, b) -> a
                        ),
                        Map::putAll
                );
    }

    /**
     * Check if class is private
     *
     * @param clazz checked class
     * @return true if class is private
     */
    private static boolean isPrivate(Class<?> clazz) {
        return Modifier.isPrivate(clazz.getModifiers());
    }

    /**
     * Throws if one of parameter of executable are private, otherwise does nothing
     *
     * @param executable checked executable
     * @throws ImplerException throws if executable cannot be implemented
     */
    private static void privateArgumentsCheck(Executable executable) throws ImplerException {
        if (
                Arrays.stream(executable.getParameters())
                    .map(Parameter::getType)
                    .anyMatch(Implementor::isPrivate)
        ) {
            throw new ImplerException("Cannot create method with private argument types");
        }
    }

    /**
     * Write the class implementation (from is "extends" or "implement" depending on whether it is implemented a class or interface)
     *
     * @param from expected "extends" or "implements"
     * @param token implemented class
     * @param writer where the result will be written
     * @throws IOException error in writing
     * @throws ImplerException error in implementing methods (cannot choose overloading, return type or parameter types are private) and constructors (parameters types are private)
     */
    private static void implement(String from, Class<?> token, Writer writer) throws IOException, ImplerException {
        writer.write(setUTF8Characters("package " + token.getPackageName() + ";" + System.lineSeparator() + System.lineSeparator()));
        String name = token.getSimpleName() + "Impl";
        writer.write(setUTF8Characters("public class " + name + " " + from + " " + getName(token) + " {" + System.lineSeparator()));
        writer.write(setUTF8Characters(implementConstructors(name, getConstructors(token))));
        writer.write(setUTF8Characters(implementMethods(getMethods(token))));
        writer.write(System.lineSeparator() + "}");
    }

    /**
     * Build string of constructors
     *
     * @param name class name
     * @param constructors map from overloading of constructor to constructor
     * @return built string
     * @throws ImplerException if one of the constructors has parameter with private type
     */
    private static String implementConstructors(String name, Map<String, Constructor<?>> constructors) throws ImplerException {
        StringBuilder resBuilder = new StringBuilder();
        for (Map.Entry<String, Constructor<?>> entry : constructors.entrySet()) {
            privateArgumentsCheck(entry.getValue());
            resBuilder
                    .append("public ").append(name).append('(').append(entry.getKey()).append(")")
                    .append(exceptionsToString(entry.getValue().getExceptionTypes()))
                    .append("{").append(System.lineSeparator())
                    .append("super(")
                    .append(
                            Arrays.stream(entry.getValue().getParameters())
                            .map(Parameter::getName)
                            .collect(Collectors.joining(", "))
                    ).append(");").append(System.lineSeparator()).append("}").append(System.lineSeparator())
                    .append(System.lineSeparator());
        }
        return resBuilder.toString();
    }

    /**
     * Build string with declaration of throwing exceptions
     *
     * @param exceptions classes that can be thrown
     * @return built string
     */
    private static String exceptionsToString(Class<?>[] exceptions) {
        if (exceptions.length == 0) {
            return " ";
        }
        return " throws " + Arrays.stream(exceptions)
                .map(Implementor::getName)
                .collect(Collectors.joining(", ")) + " ";
    }

    /**
     * Build string of methods
     *
     * @param methods map from overloading of method to method
     * @return built string
     * @throws ImplerException if one of the methods has private return of parameter type
     */
    private static String implementMethods(Map<String, Method> methods) throws ImplerException {
        StringBuilder resBuilder = new StringBuilder();
        for (Map.Entry<String, Method> entry : methods.entrySet()) {
            privateArgumentsCheck(entry.getValue());
            if (isPrivate(entry.getValue().getReturnType())) {
                throw new ImplerException("Cannot create method with private return type");
            }
            resBuilder
                    .append("public ").append(getName(entry.getValue().getReturnType()))
                    .append(" ").append(entry.getKey())
                    .append(" {").append(System.lineSeparator())
                    .append("return ").append(getDefaultValue(entry.getValue().getReturnType().getName()))
                    .append(";").append(System.lineSeparator()).append("}").append(System.lineSeparator())
                    .append(System.lineSeparator());
        }
        return resBuilder.toString();
    }

    /**
     * Return name of type that can be written in code (replace '$' in inner classes to '.')
     *
     * @param type type which name will generate
     * @return name of type
     */
    private static String getName(Type type) {
        Class<?> clazz = (Class<?>) type;
        if (clazz.isPrimitive() || clazz.isArray()) {
            return type.getTypeName();
        }
        List<String> className = new LinkedList<>();
        while (clazz != null) {
           className.addFirst(clazz.getSimpleName());
           clazz = clazz.getDeclaringClass();
        }
        className.addFirst(((Class<?>) type).getPackageName());
        return String.join(".", className);
    }

    /**
     * Replace all non ASCII characters to unicode
     *
     * @param string formatted string
     * @return formatted string
     */
    private static String setUTF8Characters(String string) {
        final char[] chars = string.toCharArray();
        StringBuilder result = new StringBuilder();
        for (char aChar : chars) {
            if ((int) aChar <= 127) {
                result.append(aChar);
            } else {
                result.append(String.format("\\u%04x", (int) aChar));
            }
        }
        return result.toString();
    }

    /**
     * Return default value of type by his name
     *
     * @return default value of this type in string format
     */
    private static String getDefaultValue(String typename) {
        return switch (typename) {
            case "int", "byte", "short" -> "0";
            case "long" -> "0L";
            case "float" -> "0.0f";
            case "double" -> "0.0d";
            case "char" -> "'\\u0000'";
            case "boolean" -> "false";
            case "void" -> "";
            case null, default -> "null";
        };
    }

    /**
     * Throws if class cannot be implemented, otherwise does nothing
     * <p>
     * Class cannot be implemented if
     * <ul>
     *     <li>It is an array</li>
     *     <li>It is a primitive type (int, float, boolean, ...)</li>
     *     <li>It is a final or private</li>
     *     <li>It is an enum</li>
     *     <li>It is a {@link java.lang.Record} or {@link java.lang.Enum}</li>
     * </ul>
     *
     * @param token checked class
     * @throws ImplerException if class cannot be implemented
     */
    private static void checkCorrectness(Class<?> token) throws ImplerException {
        if (token.isArray()) {
            throw new ImplerException("Cannot implement an array");
        } else if (token.isPrimitive()) {
            throw new ImplerException("Cannot implement a primitive type");
        } else if (Modifier.isFinal(token.getModifiers())) {
            throw new ImplerException("Cannot implement a final type");
        } else if (Modifier.isPrivate(token.getModifiers())) {
            throw new ImplerException("Cannot implement a private");
        } else if (token.isEnum()) {
            throw new ImplerException("Cannot implement an enum");
        } else if (token == Enum.class) {
            throw new ImplerException("Cannot implement a java.lang.Enum");
        } else if (token == Record.class) {
            throw new ImplerException("Cannot implement a java.lang.Record");
        }
    }

    /**
     * Compile files to classpath
     *
     * @param root root directory
     * @param files compiled files
     * @param classPath path to result file
     * @throws ImplerException if a compilation error occurred
     */
    private static void compile(final Path root, final List<String> files, final String classPath) throws ImplerException {
        final JavaCompiler compiler = ToolProvider.getSystemJavaCompiler();
        final String classpath = root + File.pathSeparator + classPath;
        final String[] args = Stream.concat(files.stream(), Stream.of("-cp", classpath, "-encoding", StandardCharsets.UTF_8.name())).toArray(String[]::new);
        ByteArrayOutputStream errorStream = new ByteArrayOutputStream();
        if (compiler.run(null, null, errorStream, args) != 0) {
            throw new ImplerException("Cannot compile code: " + errorStream.toString(StandardCharsets.UTF_8));
        }
    }

    /**
     * Create a file with implementation of interface or class
     *
     * @param token type token to create implementation for.
     * @param root root directory.
     * @throws ImplerException if you cannot implement a class
     */
    @Override
    public void implement(Class<?> token, Path root) throws ImplerException {
        checkCorrectness(token);
        try (
                Writer writer = Files.newBufferedWriter(createFile(packageToPath(token, root, "java")), StandardCharsets.UTF_8)
        ) {
            if (token.isInterface()) {
                implement("implements", token, writer);
            } else {
                if (
                        Arrays.stream(token.getDeclaredConstructors())
                                .map(Constructor::getModifiers)
                                .allMatch(Modifier::isPrivate)
                ) {
                    throw new ImplerException("Cannot implement a class without public constructors");
                }
                implement("extends", token, writer);
            }
        } catch (IOException | SecurityException exception) {
            throw new ImplerException("Cannot write: " + exception.getMessage());
        }
    }

    /**
     * Implement class or interface and write result to <var>.jar</var> file
     *
     * @param token type token to create implementation for.
     * @param jarFile target <var>.jar</var> file.
     * @throws ImplerException if you cannot implement a class
     */
    @Override
    public void implementJar(Class<?> token, Path jarFile) throws ImplerException {
        implement(token, jarFile.getParent());
        try (
                JarOutputStream out = new JarOutputStream(new FileOutputStream(createFile(jarFile).toString()), new Manifest())
        ) {
            compile(
                    jarFile.getParent(),
                    List.of(packageToPath(token, jarFile.getParent(), "java").toString()),
                    Path.of(token.getProtectionDomain().getCodeSource().getLocation().toURI()).toString()
            );
            out.putNextEntry(new ZipEntry(token.getPackageName().replace('.', '/') + "/" + token.getSimpleName() + "Impl.class"));
            Files.copy(packageToPath(token, jarFile.getParent(), "class"), out);
            out.closeEntry();
        } catch (SecurityException | IOException exception) {
            throw new ImplerException("Cannot write to file: " + exception.getMessage());
        } catch (URISyntaxException e) {
            throw new ImplerException(e.toString());
        }
    }

    /**
     * Implement a class or interface to <var>.java</var> or <var>.jar</var> file
     * <p>
     * If result file ends with <var>.jar</var> search class by name and write implementation to this jar file.
     * Otherwise, create <var>.java</var> file with implementation
     *
     * @param args first value is implemented class name, second is path to result file
     */
    public static void main(String[] args) {
        if (args == null || args.length < 2 || args[0] == null || args[1] == null) {
            System.err.println("Incorrect arguments: first argument must be a class name need to be implemented, second argument must be a path");
            return;
        }
        try {
            Class<?> clazz = Class.forName(args[0]);
            if (args[1].endsWith(".jar")) {
                new Implementor().implementJar(clazz, Path.of(args[0]));
            } else {
                new Implementor().implement(clazz, Path.of(args[0]));
            }
        } catch (ImplerException | ClassNotFoundException exception) {
            System.err.println(exception.getMessage());
        }
    }
}
