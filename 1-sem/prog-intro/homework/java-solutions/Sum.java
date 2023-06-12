public class Sum {
	public static void main(String[] args) {
		int result = 0;
		for (int i = 0; i < args.length; i++) {
			int begin = -1;
			for (int j = 0; j < args[i].length(); j++) {
				if (Character.isDigit(args[i].charAt(j)) || args[i].charAt(j) == '-') {
					if (begin == -1) {
						begin = j;
					}
				} else if (begin != -1) {
					result += Integer.parseInt(args[i].substring(begin, j));
					begin = -1;
				}
			}
			if (begin != -1) {
				result += Integer.parseInt(args[i].substring(begin));
			}
		}
		System.out.println(result);
	}
}
