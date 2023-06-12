import java.util.HashMap;
import java.util.ArrayList;
import java.io.BufferedWriter;
import java.io.OutputStreamWriter;
import java.io.FileOutputStream;
import java.io.IOException;

public class WordStatInput {
	public static void main(String[] args) {
		HashMap<String, Integer> wordStat = new HashMap();
		ArrayList<String> words = new ArrayList();
		
		if (args.length < 2) {
			System.out.println("Input output filenames not entered");
			return;
		}
		try {
			MyScanner scanner = new MyScanner(args[0], "UTF8");
			try {
				while (scanner.hasNextLine()) {
					String line = scanner.nextLine();
					int beginOfWord = -1;
					for (int i = 0; i <= line.length(); i++) {
						if (i < line.length() && 
							(Character.getType(line.charAt(i)) == Character.DASH_PUNCTUATION ||
							Character.isLetter(line.charAt(i)) ||
							line.charAt(i) == '\'')) {
								if (beginOfWord < 0) {
									beginOfWord = i;
								}
						} else if (beginOfWord >= 0) {
							String word = line.substring(beginOfWord, i).toLowerCase();
							
							if (wordStat.containsKey(word)) {
								wordStat.put(word, wordStat.get(word) + 1);
							} else {
								wordStat.put(word, 1);
								words.add(word);
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
				"UTF8"
			));
			try {
				for (int i = 0; i < words.size(); i++) {
					writer.write(words.get(i) + " " + Integer.toString(wordStat.get(words.get(i))));
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