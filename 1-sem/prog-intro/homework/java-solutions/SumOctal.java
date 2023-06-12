public class SumOctal {
	public static void main(String[] args) {
		int result = 0;
		for (int i = 0; i < args.length; i++) {
			int begin = -1;
			for (int j = 0; j < args[i].length(); j++) {
				if (args[i].charAt(j) == 'o' || args[i].charAt(j) == 'O') {
					if (begin != -1) {
					result += Integer.parseInt(args[i].substring(begin, j), 8);
					begin = -1;
					}
				} else if (Character.isWhitespace(args[i].charAt(j))) {
					if (begin != -1) {
						result += Integer.parseInt(args[i].substring(begin, j));
						begin = -1;
					}
				} else if (begin == -1) {
						begin = j;
				}
			}
			if (begin != -1) {
				result += Integer.parseInt(args[i].substring(begin));
			}
		}
		System.out.println(result);
	}
}