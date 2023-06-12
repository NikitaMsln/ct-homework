import java.util.Arrays;
import java.lang.IndexOutOfBoundsException;

class IntList {
	int[] array;
	int size;
	
	public IntList() {
		array = new int[10];
		size = 0;
	}
	
	public IntList(int[] array) {
		size = array.length;
		this.array = new int[array.length + 10];
		for (int i = 0; i < size; i++) {
			this.array[i] = array[i];
		}
	}
	
	public IntList(IntList that) {
		this.size = that.size;
		this.array = new int[that.array.length];
		for (int i = 0; i < this.size; i++) {
			this.array[i] = that.array[i];
		}
	}
	
	public void add(int value) {
		if (size >= array.length - 1) {
			array = Arrays.copyOf(array, array.length * 2);
		}
		array[size] = value;
		size++;
	}
	
	public int get(int index) throws IndexOutOfBoundsException {
		if (index >= size) {
			throw new IndexOutOfBoundsException();
		}
		return array[index];
	}
	
	public void set(int index, int value) throws IndexOutOfBoundsException{
		if (index >= size) {
			throw new IndexOutOfBoundsException();
		}
		array[index] = value;
	}
	
	public int size() {
		return size;
	}
}