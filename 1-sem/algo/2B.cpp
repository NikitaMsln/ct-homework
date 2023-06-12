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
		while (i > 0 && array[i] < array[(i-1)/2]) {
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
			if (2 * i + 2 >= size || array[2*i+1] < array[2*i+2]) j = 2*i+1;
			else j = 2*i+2;
			if (array[j] > array[i]) break;
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
	vector<int> t(100000);
	vector<vector<int>> res;
	Heap<int> h;
	for (int i = 1; i <= n; i++)
		h.insert(i);

	char p;
	int v;

	while (cin >> p) {
		cin >> v;
		if (p == '+') {
			if (n <= 0) {
				cout << 0 << " " << v << "\n";
				return 0;
			}
			t[v] = h.extract();
			vector<int> a(2);
			a[0] = t[v];
			a[1] = v;
			res.push_back(a);
			n--;
		} else {
			h.insert(t[v]);
			t[v] = 0;
			n++;
		}
	}
	for (int i = 0; i < res.size(); i++)
		cout << res[i][1] << ' ' << res[i][0] << '\n';
}