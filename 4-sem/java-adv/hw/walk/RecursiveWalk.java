package walk;

import java.io.InputStream;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

public class RecursiveWalk {
    public static void main(String[] args) {
        if (args == null || args.length < 2 || args[0] == null || args[1] == null) {
            System.out.println("Input output filenames not entered");
            return;
        }
        HashEvaluator<InputStream> evaluator;
        if (args.length >= 3 && "sha-1".equalsIgnoreCase(args[2])) {
            try {
                MessageDigest digest = MessageDigest.getInstance("SHA-1");
                evaluator = new SHAHashEvaluator(digest);
            } catch (NoSuchAlgorithmException exception) {
                System.out.println("Unsupported hash: " + args[2]);
                evaluator = new JenkinsHash();
            }
        } else {
            evaluator = new JenkinsHash();
        }
        new Walker(System.err, true).walk(args[0], args[1], evaluator);
    }
}

