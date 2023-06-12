import java.util.HashMap;
import java.util.ArrayList;
import java.util.List;
import java.util.Collections;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.FileInputStream;
import java.io.BufferedWriter;
import java.io.OutputStreamWriter;
import java.io.FileOutputStream;
import java.io.IOException;

public class WordStatWordsShingles {
    public static void main(String[] args) {
        // :NOTE: raw types
        List<String> words = new ArrayList();
        
        if (args.length < 2) {
            System.out.println("Input output filenames not entered");
            return;
        }
        
        try {
            BufferedReader reader = new BufferedReader(new InputStreamReader(
                new FileInputStream(args[0]),
                "UTF8"
            ));
            try {
                String line = reader.readLine();
                
                while (line != null) {
                    int beginOfWord = -1;
                    for (int i = 0; i <= line.length(); i++) {
                        if (i < line.length() && (
                            Character.getType(line.charAt(i)) == Character.DASH_PUNCTUATION ||
                            Character.isLetter(line.charAt(i)) ||
                            line.charAt(i) == '\''
                        )) {
                            if (beginOfWord < 0) {
                                beginOfWord = i;
                            }
                        } else if (beginOfWord >= 0) {
                            String word = line.substring(beginOfWord, i).toLowerCase();
                            if (word.length() >= 3) {
                                for (int j = 0; j < word.length() - 2; j++) {
                                    words.add(word.substring(j, j + 3));
                                }
                            } else {
                                words.add(word);
                            }
                            beginOfWord = -1;
                        }
                    }
                    
                    line = reader.readLine();
                }
            } finally {
                reader.close();
            }
        } catch (IOException e) {
            System.out.println("Input file reading error: " + e.getMessage());
        }
        
        Collections.sort(words);
        
        try {
            BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(
                new FileOutputStream(args[1]),
                "UTF8"
            ));
            try {
                int count = 0, firstWord = 0;
                for (int i = 0; i <= words.size(); i++, count++) {
                    if (i == words.size() || !words.get(i).equals(words.get(firstWord))) {
                        writer.write(words.get(firstWord) + " " + Integer.toString(count));
                        writer.newLine();
                        firstWord = i;
                        count = 0;
                    }
                }
            } finally {
                writer.close();
            }
        } catch (IOException e) {
            System.out.println("Output file writing error: " + e.getMessage());
        }
    }
}
