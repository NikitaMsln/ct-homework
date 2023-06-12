public class SumLongOctal {
	public static void main(String[] args) {
		long result = 0;
		for (int i = 0; i < args.length; i++) {
			int begin = -1;
			for (int j = 0; j <= args[i].length(); j++) {
				if (j == args[i].length() || Character.isWhitespace(args[i].charAt(j))) {
					if (begin != -1) {
						result += Long.parseLong(args[i].substring(begin, j));
						begin = -1;
					}
				} else if (args[i].charAt(j) == 'o' || args[i].charAt(j) == 'O') {
					if (begin != -1) {
						result += Long.parseLong(args[i].substring(begin, j), 8);
						begin = -1;
					}
				} else if (begin == -1) {
						begin = j;
				}
			}
		}
		System.out.println(result);
	}
}