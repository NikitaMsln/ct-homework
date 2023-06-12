#include <bits/stdc++.h>

using namespace std;

template <typename T>
class Heap {
private:
	T* array;
	int size, realSize;

	void resize() {
		realSize *= 2;
		T* newArray = new T[realSize];
		for (int i = 0; i < size; i++)
			newArray[i] = array[i];
		delete[] array;
		array = newArray;
	}

public:
	Heap() {
		realSize = 8;
		array = new T[realSize];
		size = 0;
	}

	~Heap() {
		delete[] array;
	}

	void insert(T value) {
		if (realSize <= size)
			resize();
		int i = size;
		array[i] = value;
		size++;
		while (i > 0 && array[i] > array[(i-1)/2]) {
			T t = array[i];
			array[i] = array[(i-1)/2];
			array[(i-1)/2] = t;
			i = (i-1)/2;
		}
	}

	T extract() {
		T res = array[0];
		array[0] = array[size-1];
		size--;
		int i = 0;
		while (2 * i + 1 < size) {
			int j;
			if (2 * i + 2 >= size || array[2*i+1] > array[2*i+2]) j = 2*i+1;
			else j = 2*i+2;
			if (array[j] < array[i]) break;
			T t = array[i];
			array[i] = array[j];
			array[j] = t;
			i = j;
		}
		return res;
	}
};

int main() {
	int n;
	cin >> n;
	Heap<int> h;
	while (n--) {
		int a, b;
		cin >> a;
		if (a) {
			cout << h.extract() << "\n";
		} else {
			cin >> b;
			h.insert(b);
		}
	}
}