package walk;

public class Walk {
    public static void main(String[] args) {
        if (args == null || args.length < 2 || args[0] == null || args[1] == null) {
            System.out.println("Input output filenames not entered");
            return;
        }
        new Walker(System.err, false).walk(args[0], args[1], new JenkinsHash());
    }
}

