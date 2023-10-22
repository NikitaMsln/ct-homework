#include <bits/stdc++.h>

using namespace std;

class SegmentTree {
private:
	vector<int32_t> data;
	vector<int32_t[2]> bounds;
	int32_t size;
	int32_t data_size;

	inline static int32_t minimum(int32_t x, int32_t y) { return (x < y)? x : y; }
public:
	explicit SegmentTree(const vector<int32_t>& array) {
		data_size = 1;
		for (; data_size < array.size(); data_size *= 2);

		int32_t half_data_size = data_size;
		data_size *= 2;

		size = array.size();
		data = vector<int32_t>(data_size);
		bounds = vector<int32_t[2]>(data_size);

		for (int32_t i = data_size - 1; i >= half_data_size; i--) {
			if (i >= half_data_size + size) data[i] = INT32_MAX;
			else data[i] = array[i - half_data_size];
			bounds[i][0] = i - half_data_size;
            bounds[i][1] = bounds[i][0] + 1;
		}

		for (int32_t i = half_data_size - 1; i > 0; i--) {
			data[i] = minimum(data[i * 2], data[i * 2 + 1]);
			bounds[i][0] = bounds[i * 2][0];
			bounds[i][1] = bounds[i * 2 + 1][1];
		}
	}

	void set(int32_t index, int32_t value) {
        if (index < 0 || index >= size) {
            throw exception();
        }
		index += data_size / 2;
		data[index] = value;
		while (index > 1) {
			index =  index / 2;
			data[index] = minimum(data[index * 2], data[index * 2 + 1]);
		}
	}

	int32_t min(int32_t left, int32_t right, int32_t index = 1) const {
		if (bounds[index][1] <= left || bounds[index][0] >= right) {
			return INT32_MAX;
		} else if (bounds[index][0] >= left && bounds[index][1] <= right) {
			return data[index];
		} else {
			return minimum(min(left, right, index * 2), min(left, right, index * 2 + 1));
		}
	}
};

int main() {
	int32_t n;
	cin >> n;
	vector<int32_t> array(n);
	for (int32_t i = 0; i < n; i++) {
		cin >> array[i];
	}
	SegmentTree tree(array);

	string s;
	while (cin >> s) {
		int32_t a, b;
		cin >> a >> b;
		if (s == "min") {
			cout << tree.min(a - 1, b) << "\n";
		} else {
			tree.set(a - 1, b);
		}
	}

}
