import java.util.NoSuchElementException;
import java.io.Reader;
import java.io.InputStreamReader;
import java.io.StringReader;
import java.io.InputStream;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.UnsupportedEncodingException;
import java.io.IOException;

public class MyScanner {
	private final Reader reader;
	private char[] buffer = new char [1024];
	private int nowPosition = 0;
	private int bufferSize = 0;
	private String readedNext = "";
	
	public MyScanner(InputStream source) {
		reader = new InputStreamReader(source);
	}
	
	public MyScanner(String source) {
		reader = new StringReader(source);
	}
	
	public MyScanner(String source, String encoding) throws FileNotFoundException, SecurityException, UnsupportedEncodingException {
		reader = new InputStreamReader(new FileInputStream(source), encoding);
	}
	
	private boolean checkEndBuffer() throws IOException {
		if (nowPosition >= bufferSize) {
			bufferSize = reader.read(buffer);
			nowPosition = 0;
			return bufferSize <= 0;
		}
		return false;
	}
	
	private String getValue() throws IOException {
		while (nowPosition >= bufferSize || Character.isWhitespace(buffer[nowPosition])) {
			if (nowPosition >= bufferSize) {
				if (checkEndBuffer()) {
					break;
				}
			} else {
				nowPosition++;
			}
		}
		StringBuilder result = new StringBuilder();
		while (nowPosition >= bufferSize || !Character.isWhitespace(buffer[nowPosition])) {
			if (nowPosition >= bufferSize) {
				if (checkEndBuffer()) {
					break;
				}
			} else {
				result.append(buffer[nowPosition]);
				nowPosition++;
			}
		}
		return result.toString();
	}
	
	public boolean hasNext() {
		if (readedNext.length() > 0) {
			return true;
		}
		
		try {
			readedNext = getValue();
		} catch (IOException e) {
			return false;
		}
		return readedNext.length() > 0;
	}
	
	public boolean hasNextLine() {
		if (nowPosition >= bufferSize) {
			try {
				if (checkEndBuffer()) {
					return false;
				}
			} catch (IOException e) {
				return false;
			}
		}
		return true;
	}
	
	public String next() throws NoSuchElementException, IOException {
		if (readedNext.length() > 0) {
			String result = readedNext;
			readedNext = "";
			return result;
		} else {
			String result = getValue();
			if (result.length() > 0) {
				return result;
			} else {
				throw new NoSuchElementException("Next value not found");
			}
		}
	}
	
	public int nextInt() throws NoSuchElementException, NumberFormatException, IOException {
		return Integer.parseInt(next());
	}
	
	public String nextLine() throws NoSuchElementException, IOException {
		if (checkEndBuffer()) {
			throw new NoSuchElementException("Next line not found");
		}
		StringBuilder result = new StringBuilder(readedNext);
		readedNext = "";
		while (true) {
			if (checkEndBuffer()) {
				break;
			}
			if (buffer[nowPosition] == '\n' || buffer[nowPosition] == '\r') {
				
				char lastSymb = buffer[nowPosition];
				nowPosition++;
				
				if (lastSymb == '\r' && System.lineSeparator().equals("\r\n")) {
					nowPosition++;
					if (!checkEndBuffer() && buffer[nowPosition] == '\n') {
						nowPosition++;
					}
				}
				break;
			} else {
				result.append(buffer[nowPosition]);
				nowPosition++;
			}
		}
		return result.toString();
	}
	
	public void close() throws IOException {
		reader.close();
	}
}