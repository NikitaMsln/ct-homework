#include <bits/stdc++.h>

using namespace std;

class segment_tree {
private:
    struct data {
        int max_size = 0;
        long long left_value = 0;
        long long right_value = 0;
        int size = 0;
        int left_size = 0;
        int right_size = 0;
        bool left_is_up = true;
        bool right_is_down = true;
    };

    static data empty_data;

    vector<data> data_;
    vector<int[2]> bounds_;
    int size_;
    int data_size_;

    static data concat(const data &left, const data &right) {
        if (left.size == 0) {
            return right;
        } else if (right.size == 0) {
            return left;
        }
        data result;
        result.left_value = left.left_value;
        result.right_value = right.right_value;
        result.left_size = left.left_size;
        result.right_size = right.right_size;
        result.left_is_up = left.left_is_up;
        result.right_is_down = right.right_is_down;
        result.size = left.size + right.size;
        int concat_value = 0;
        if (left.right_value > right.left_value) {
            if (right.left_is_up) {
                concat_value = left.right_size + 1;
            } else {
                concat_value = left.right_size + right.left_size;
            }
            if (right.right_size == right.size) {
                if (right.size == 1) {
                    result.right_is_down = true;
                }
                if (!right.left_is_up) {
                    result.right_size = concat_value;
                }
            }
            if (left.left_size == left.size) {
                if (left.size == 1) {
                    result.left_is_up = false;
                }
                result.left_size = concat_value;
            }
        } else if (left.right_value < right.left_value) {
            if (left.right_is_down) {
                concat_value = right.left_size + 1;
            } else {
                concat_value = left.right_size + right.left_size;
            }
            if (right.right_size == right.size) {
                if (right.size == 1) {
                    result.right_is_down = false;
                }
                result.right_size = concat_value;
            }
            if (left.left_size == left.size) {
                if (left.size == 1) {
                    result.left_is_up = true;
                }
                if (!left.right_is_down) {
                    result.left_size = concat_value;
                }
            }
        }
        result.max_size = std::max(concat_value, std::max(left.max_size, right.max_size));
        return result;
    }

    data get_segment(int left, int right, int index = 1) const {
        if (bounds_[index][1] <= left || bounds_[index][0] >= right) {
            return data();
        } else if (bounds_[index][0] >= left && bounds_[index][1] <= right) {
            return data_[index];
        } else {
            return concat(get_segment(left, right, index * 2), get_segment(left, right, index * 2 + 1));
        }
    }

public:
    explicit segment_tree(const vector<long long> &array) {
        data_size_ = 1;
        for (; data_size_ < array.size(); data_size_ *= 2);

        int half_data_size_ = data_size_;
        data_size_ *= 2;

        size_ = array.size();
        data_ = vector<data>(data_size_);
        bounds_ = vector<int[2]>(data_size_);

        for (int i = data_size_ - 1; i >= half_data_size_; i--) {
            if (i < half_data_size_ + size_) {
                data_[i].left_value = data_[i].right_value = array[i - half_data_size_];
                data_[i].size = data_[i].max_size = data_[i].left_size = data_[i].right_size = 1;
                data_[i].left_is_up = false;
                data_[i].right_is_down = false;
            }
            bounds_[i][0] = i - half_data_size_;
            bounds_[i][1] = bounds_[i][0] + 1;
        }

        for (int i = half_data_size_ - 1; i > 0; i--) {
            data_[i] = concat(data_[i * 2], data_[i * 2 + 1]);
        }
    }

    void set(int index, long long value) {
        if (index < 0 || index >= size_) {
            throw exception();
        }
        index += data_size_ / 2;
        data_[index].left_value = data_[index].right_value = value;
        while (index > 1) {
            index = index / 2;
            data_[index] = concat(data_[index * 2], data_[index * 2 + 1]);
        }
    }

    int max(int left, int right) const {
        return get_segment(left, right).max_size;
    }

    int max() const {
        return data_[1].max_size;
    }
};

int main() {
    int n;
    cin >> n;
    vector<long long> a(n);
    for (int i = 0; i < n; i++) {
        cin >> a[i];
    }
    segment_tree st(a);
    int m;
    cin >> m;
    while (m--) {
        long long x, y;
        cin >> x >> y;
        st.set(x - 1, y);
        cout << st.max() << "\n";
    }
}