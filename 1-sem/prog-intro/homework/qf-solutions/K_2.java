import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Scanner;

public class KTask {
    public static void main(String[] args) {
        Scanner in = new Scanner(System.in);

        int n = in.nextInt(), m = in.nextInt();
        char country[][] = new char[n][m];
        int[] coordA = new int[2];
        List<int[]> otherLetters = new ArrayList<>(), verticalBordersOtherLetters = new ArrayList<>();
        int leftBorderByHeight[] = new int[n], rightBorderByHeight[] = new int[n];

        for (int i = 0; i < n; i++) {
            String line = in.next();
            for (int j = 0; j < m; j++) {
                country[i][j] = line.charAt(j);
                if (country[i][j] == 'A') {
                    coordA[0] = i;
                    coordA[1] = j;
                } else if (country[i][j] != '.') {
                    otherLetters.add(new int[] {i, j});
                }
            }
        }

        // Fill 'a'

        // Find coords of max width in line
        // In line with A
        for (int i = coordA[1]; i >= 0; i--) {
            if (i == 0 || country[coordA[0]][i - 1] != '.') {
                leftBorderByHeight[coordA[0]] = i;
                break;
            }
        }

        for (int i = coordA[1]; i < m; i++) {
            if (i + 1 == m || country[coordA[0]][i + 1] != '.') {
                rightBorderByHeight[coordA[0]] = i;
                break;
            }
        }

        // In other lines
        for (int i = coordA[0] - 1; i >= 0; i--) {
            if (country[i][coordA[1]] != '.') {
                for (int j = 0; j <= i; j++) {
                    leftBorderByHeight[j] = -1;
                    rightBorderByHeight[j] = -1;
                }
                break;
            }

            for (int j = coordA[1]; j >= leftBorderByHeight[i + 1]; j--) {
                if (j == leftBorderByHeight[i + 1] || country[i][j - 1] != '.') {
                    leftBorderByHeight[i] = j;
                    break;
                }
            }

            for (int j = coordA[1]; j <= rightBorderByHeight[i + 1]; j++) {
                if (j == rightBorderByHeight[i + 1] || country[i][j + 1] != '.') {
                    rightBorderByHeight[i] = j;
                    break;
                }
            }
        }


        for (int i = coordA[0] + 1; i < n; i++) {
            if (country[i][coordA[1]] != '.') {
                for (int j = n - 1; j >= i; j--) {
                    leftBorderByHeight[j] = -1;
                    rightBorderByHeight[j] = -1;
                }
                break;
            }

            for (int j = coordA[1]; j >= leftBorderByHeight[i - 1]; j--) {
                if (j == leftBorderByHeight[i - 1] || country[i][j - 1] != '.') {
                    leftBorderByHeight[i] = j;
                    break;
                }
            }

            for (int j = coordA[1]; j <= rightBorderByHeight[i - 1]; j++) {
                if (j == rightBorderByHeight[i - 1] || country[i][j + 1] != '.') {
                    rightBorderByHeight[i] = j;
                    break;
                }
            }
        }

        // Find rectangle with max S
        int maxS = 0, beginMaxRect = -1, endMaxRect = -1;
        for (int i = 0; i <= coordA[0]; i++) {
            if (leftBorderByHeight[i] == -1) {
                continue;
            }
            for (int j = coordA[0]; j < n; j++) {
                if (j == n - 1 || rightBorderByHeight[j + 1] < rightBorderByHeight[i] ||
                        leftBorderByHeight[j + 1] > leftBorderByHeight[i] || leftBorderByHeight[j + 1] == -1) {
                    int S = (rightBorderByHeight[i] - leftBorderByHeight[i] + 1) * (j - i + 1);
                    if (S > maxS) {
                        maxS = S;
                        beginMaxRect = i;
                        endMaxRect = j;
                    }
                    break;
                }
            }
        }

        for (int i = n-1; i >= coordA[0]; i--) {
            if (leftBorderByHeight[i] == -1) {
                continue;
            }
            for (int j = coordA[0]; j >= 0; j--) {
                if (j == 0 || rightBorderByHeight[j - 1] < rightBorderByHeight[i] ||
                        leftBorderByHeight[j - 1] > leftBorderByHeight[i] || leftBorderByHeight[j - 1] == -1) {
                    int S = (rightBorderByHeight[i] - leftBorderByHeight[i] + 1) * (i - j + 1);
                    if (S > maxS) {
                        maxS = S;
                        beginMaxRect = j;
                        endMaxRect = i;
                    }
                    break;
                }
            }
        }

        // Put 'a'
        for (int i = beginMaxRect; i <= endMaxRect; i++) {
            int start = Math.max(leftBorderByHeight[beginMaxRect], leftBorderByHeight[endMaxRect]),
                end = Math.min(rightBorderByHeight[beginMaxRect], rightBorderByHeight[endMaxRect]);
            for (int j = start; j <= end; j++) {
                if (country[i][j] != 'A') {
                    country[i][j] = 'a';
                }
            }
        }

        // Fill other letters

        // Find vertical lines of rectangle
        for (int[] letter : otherLetters) {
            int fromLine, toLine;
            char template = Character.toLowerCase(country[letter[0]][letter[1]]);
            for (fromLine = letter[0]; fromLine >= 0; fromLine--) {
                if (fromLine == 0 || country[fromLine - 1][letter[1]] != '.') {
                    break;
                } else {
                    country[fromLine - 1][letter[1]] = template;
                }
            }
            for (toLine = letter[0]; toLine < n; toLine++) {
                if (toLine == n - 1 || country[toLine + 1][letter[1]] != '.') {
                    break;
                } else {
                    country[toLine + 1][letter[1]] = template;
                }
            }

            verticalBordersOtherLetters.add(new int[] {fromLine, toLine});
        }

        // Fill rectangles by vertical lines
        for (int i = 0; i < otherLetters.size(); i++) {
            char template = Character.toLowerCase(country[otherLetters.get(i)[0]][otherLetters.get(i)[1]]);

            boolean toNextCol = true;
            for (int j = otherLetters.get(i)[1]; j >= 0 && toNextCol; j--) {
                toNextCol = toNextCol && j > 0;
                for (int k = verticalBordersOtherLetters.get(i)[0]; k <= verticalBordersOtherLetters.get(i)[1]; k++) {
                    if (toNextCol) {
                        toNextCol = toNextCol && country[k][j-1] == '.';
                    }
                    if (country[k][j] == '.') {
                        country[k][j] = template;
                    }
                }
            }

            toNextCol = true;
            for (int j = otherLetters.get(i)[1]; j < m && toNextCol; j++) {
                toNextCol = toNextCol && j < m - 1;
                for (int k = verticalBordersOtherLetters.get(i)[0]; k <= verticalBordersOtherLetters.get(i)[1]; k++) {
                    if (toNextCol) {
                        toNextCol = toNextCol && country[k][j+1] == '.';
                    }
                    if (country[k][j] == '.') {
                        country[k][j] = template;
                    }
                }
            }

        }

        for (int i = 0; i < n; i++) {
            System.out.println(country[i]);
        }
    }
}