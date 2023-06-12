import java.util.Scanner;
import java.util.Arrays;

public class ReverseAvg {
	public static void main(String[] args) {
		Scanner scanner = new Scanner(System.in);
		
		int countLines = 0;
		int[][] numbers = new int[10][];
		int[] countNumbersInLine = new int[10];
		
		long[] colSums = new long[10], rowSums = new long[10];
		int[] countNumbersInCol = new int[10];

		for (int i = 0; scanner.hasNextLine(); i++, countLines++) {
			Scanner lineScanner = new Scanner(scanner.nextLine());
			int[] lineNumbers = new int[10];
			
			if (i >= numbers.length) {
				numbers = Arrays.copyOf(numbers, numbers.length * 2); 
				countNumbersInLine = Arrays.copyOf(countNumbersInLine, countNumbersInLine.length * 2);
				rowSums = Arrays.copyOf(rowSums, rowSums.length * 2);
			}
			
			for (int j = 0; lineScanner.hasNext(); j++, countNumbersInLine[i]++) {
				if (j >= lineNumbers.length) {
					lineNumbers = Arrays.copyOf(lineNumbers, lineNumbers.length * 2);
				}
				if (j >= colSums.length) {
					colSums = Arrays.copyOf(colSums, colSums.length * 2);
					countNumbersInCol = Arrays.copyOf(countNumbersInCol, countNumbersInCol.length * 2);
				}
				
				lineNumbers[j] = lineScanner.nextInt();
				rowSums[i] += lineNumbers[j];
				colSums[j] += lineNumbers[j];
				countNumbersInCol[j]++;
			}
			
			numbers[countLines] = lineNumbers;
		}
		
		for (int i = 0; i < countLines; i++) {
			for (int j = 0; j < countNumbersInLine[i]; j++) {
				System.out.printf("%d ", (rowSums[i] + colSums[j] - numbers[i][j]) / 
										 (countNumbersInLine[i] + countNumbersInCol[j] - 1));
			}
			System.out.println();
		}
	}
}