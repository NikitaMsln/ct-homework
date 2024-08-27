package walk;

import java.io.PrintStream;

public class Logger {
    private final PrintStream logOut;

    public Logger(PrintStream out) {
        logOut = out;
    }

    public void log(String message) {
        if (logOut != null) {
            logOut.println(message);
        }
    }
}

