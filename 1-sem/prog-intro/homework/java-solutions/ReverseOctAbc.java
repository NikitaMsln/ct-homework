import java.util.Arrays;
import java.io.IOException;

public class ReverseOctAbc {
	public static void main(String[] args) {
		MyScanner scanner = new MyScanner(System.in);
		
		int countLines = 0;
		int[][] numbers = new int[10][];
		int[] countNumbersInLine = new int[10];
		
		try {
			for (int i = 0; scanner.hasNextLine(); i++, countLines++) {
				String ln = scanner.nextLine();
				MyScanner lineScanner = new MyScanner(ln);
				//System.err.println(ln);

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
						String nextNumber = lineScanner.next().toLowerCase();
						StringBuilder nextNumberRedacted = new StringBuilder();
						int base, size;
						int result = 0;
						boolean isLessNull = false;
						if (nextNumber.charAt(nextNumber.length() - 1) == 'o') {
							base = 8;
							size = nextNumber.length() - 1;
						} else {
							base = 10;
							size = nextNumber.length();
						}
						for (int x = 0; x < size; x++) {
							if (nextNumber.charAt(x) == '-') {
								isLessNull = true;
							} else if (Character.isLetter(nextNumber.charAt(x))) {
								result = result * base + (int)(nextNumber.charAt(x) - 'a');
							} else {
								result = result * base + (int)(nextNumber.charAt(x) - '0');
							}
						}
						if (isLessNull) {
							result *= -1;
						}
						lineNumbers[j] = result;
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