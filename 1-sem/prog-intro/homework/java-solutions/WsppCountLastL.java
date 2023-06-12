import java.nio.charset.StandardCharsets;
import java.util.Map;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.io.IOException;
import java.io.BufferedWriter;
import java.io.OutputStreamWriter;
import java.io.FileOutputStream;


public class WsppCountLastL {
    public static void main(String[] args) {
        Map<String, IntList> wordStat = new HashMap<>();
        Map<String, Integer> wordCount = new LinkedHashMap<>();
        Map<String, Integer> lastLine = new HashMap<>();

        if (args.length < 2) {
            System.out.println("Input output filenames not entered");
            return;
        }

        try {
            MyScanner scanner = new MyScanner(args[0], "UTF8");
            try {
                for (int lineNumber = 1; scanner.hasNextLine(); lineNumber++) {
                    String line = scanner.nextLine();
                    int beginOfWord = -1;
                    for (int i = 0, wordNumber = 0; i <= line.length(); i++) {
                        if (i < line.length() &&
                            (Character.getType(line.charAt(i)) == Character.DASH_PUNCTUATION ||
                                Character.isLetter(line.charAt(i)) ||
                                line.charAt(i) == '\'')) {
                            if (beginOfWord < 0) {
                                beginOfWord = i;
                            }
                        } else if (beginOfWord >= 0) {
                            String word = line.substring(beginOfWord, i).toLowerCase();
                            wordNumber++;

                            if (wordStat.containsKey(word)) {
                                wordCount.put(word, wordCount.get(word) + 1);
                                if (lastLine.get(word) == lineNumber) {
                                    wordStat.get(word).set(wordStat.get(word).size() - 1, wordNumber);
                                } else {
                                    lastLine.put(word, lineNumber);
                                    wordStat.get(word).add(wordNumber);
                                }
                            } else {
                                wordStat.put(word, new IntList(new int[]{wordNumber}));
                                wordCount.put(word, 1);
                                lastLine.put(word, lineNumber);
                            }
                            beginOfWord = -1;
                        }
                    }
                }
            } finally {
                scanner.close();
            }
        } catch (IOException e) {
            System.out.println("Input file reading error: " + e.getMessage());
        }

        try {
            BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(
                new FileOutputStream(args[1]),
                StandardCharsets.UTF_8
            ));
            try {
                List<Map.Entry<String, Integer>> sortedKeys = new ArrayList<>(wordCount.entrySet());
                sortedKeys.sort(Map.Entry.comparingByValue());
                for (Map.Entry<String, Integer> entry : sortedKeys) {
                    writer.write(entry.getKey() + " " + entry.getValue());
                    IntList lastEntries = wordStat.get(entry.getKey());
                    for (int i = 0; i < lastEntries.size(); i++) {
                        writer.write(" " + lastEntries.get(i));
                    }
                    writer.newLine();
                }
            } catch (IOException e) {
                System.out.println("Output file writing error: " + e.getMessage());
            } finally {
                writer.close();
            }
        } catch (IOException e) {
            System.out.println("Output file writing error: " + e.getMessage());
        }
    }
}
