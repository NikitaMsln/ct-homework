import java.util.Map;
import java.util.LinkedHashMap;
import java.io.IOException;
import java.io.BufferedWriter;
import java.io.OutputStreamWriter;
import java.io.FileOutputStream;
import java.io.FileInputStream;


public class Wspp {
	public static void main(String[] args) {
		Map<String, IntList> wordStat = new LinkedHashMap<>();
		
		if (args.length < 2) {
			System.out.println("Input output filenames not entered");
			return;
		}
		
		try {
			MyScanner scanner = new MyScanner(args[0], "UTF8");
			int wordsCount = 0;
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
							wordsCount++;
							
							if (wordStat.containsKey(word)) {
								wordStat.get(word).add(wordsCount);
							} else {
								wordStat.put(word, new IntList(new int[]{wordsCount}));
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
				for (Map.Entry<String, IntList> entry : wordStat.entrySet()) {
					writer.write(entry.getKey() + " " + Integer.toString(entry.getValue().size()));
					for (int i = 0; i < entry.getValue().size(); i++) {
						writer.write(" " + Integer.toString(entry.getValue().get(i)));
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