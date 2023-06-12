import java.util.Arrays;
import java.io.IOException;

public class Reverse {
	public static void main(String[] args) {
		MyScanner scanner = new MyScanner(System.in);
		
		int countLines = 0;
		int[][] numbers = new int[10][];
		int[] countNumbersInLine = new int[10];
		
		try {
			for (int i = 0; scanner.hasNextLine(); i++, countLines++) {
				String s = scanner.nextLine();
				MyScanner lineScanner = new MyScanner(s);
				try {
					int[] lineNumbers = new int[10];
					
					if (i >= numbers.length) {
						numbers = Arrays.copyOf(numbers, numbers.length * 2);
						countNumbersInLine = Arrays.copyOf(countNumbersInLine, countNumbersInLine.length * 2);
					}
					
					
					for (int j = 0; lineScanner.hasNext(); j++, countNumbersInLine[i]++) {
						if (j >= lineNumbers.length) {
							lineNumbers = Arrays.copyOf(lineNumbers, lineNumbers.length * 2);
						}
						lineNumbers[j] = lineScanner.nextInt();
					}
					
					numbers[countLines] = lineNumbers;
				} catch (IOException e) {
					System.out.println("Cannot read next number: " + e.getMessage());
				}
			}
		} catch (IOException e) {
			System.out.println("Cannot read next line: " + e.getMessage());
		}
		
		for (int i = countLines - 1; i >= 0; i--) {
			for (int j = countNumbersInLine[i] - 1; j >= 0; j--) {
				System.out.printf("%d ", numbers[i][j]);
			}
			System.out.println();
		}
	}
}