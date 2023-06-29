#include <bits/stdc++.h>

using namespace std;

class segment_tree {
public:
    segment_tree(size_t n) {
        data_size_ = 1;
        for (; data_size_ < n; data_size_ *= 2);

        size_t half_data_size_ = data_size_;
        data_size_ *= 2;

        data_ = vector<data>(data_size_);
        bounds_ = vector<size_t[2]>(data_size_);

        for (int i = data_size_ - 1; i >= half_data_size_; i--) {
            if (i < half_data_size_ + n) {
                data_[i].set(0);
                data_[i](0, 2) = data_[i](1, 2) = data_[i](2, 2) = data_[i](1, 0) = data_[i](2, 1) = 1;
                data_[i].size = 1;
            } else {
                data_[i].size = 0;
            }
            bounds_[i][0] = i - half_data_size_;
            bounds_[i][1] = bounds_[i][0] + 1;
        }

        for (int i = half_data_size_ - 1; i > 0; i--) {
            concat(i);
        }
    }

    size_t ways_count() const {
        return data_[1](0, 2);
    }

    void block(size_t index) {
        index += data_size_ / 2;
        if (data_[index](2, 2) != 0) {
            data_[index].set(0);
        } else {
            data_[index](0, 2) = data_[index](1, 2) = data_[index](2, 2) = data_[index](2, 1) = data_[index](1, 0) = 1;
        }
        while (index > 1) {
            index /= 2;
            concat(index);
        }
    }

private:
    struct data {
        array<array<size_t, 3>, 3> ways_count;
        size_t size;

        size_t &operator()(size_t from, size_t to) {
            return ways_count[from][to];
        }

        const size_t &operator()(size_t from, size_t to) const {
            return ways_count[from][to];
        }

        void set(size_t value) {
            for (size_t i = 0; i < 3; i++) {
                for (size_t j = 0; j < 3; j++) {
                    ways_count[i][j] = value;
                }
            }
        }

        data &operator=(const data &other) {
            if (this == &other) {
                return *this;
            }
            for (size_t i = 0; i < 3; i++) {
                for (size_t j = 0; j < 3; j++) {
                    operator()(i, j) = other(i, j);
                }
            }
            size = other.size;
            return *this;
        }
    };

    static const size_t MOD = 1000000007;
    vector<data> data_;
    size_t data_size_;
    vector<size_t[2]> bounds_;

    void concat(size_t index) {
        data &left = data_[index * 2], &right = data_[index * 2 + 1], &result = data_[index];
        if (left.size == 0) {
            result = right;
            return;
        } else if (right.size == 0) {
            result = left;
            return;
        }
        result.size = left.size + right.size;
        for (size_t i = 0; i < 3; i++) {
            for (size_t j = 0; j < 3; j++) {
                result(i, j) = 0;
                for (size_t x = 0; x < 3; x++) {
                    for (size_t y = 0; y < 3; y++) {
                        if (x >= y) {
                            result(i, j) = (result(i, j) + left(i, x) * right(y, j) % MOD) % MOD;
                        }
                    }
                }
            }
        }
    }
};

int main() {
    size_t n, m;
    cin >> n >> m;
    segment_tree st(n + 1);
    cout << st.ways_count() << "\n";
    while (m--) {
        size_t index;
        cin >> index;
        st.block(index);
        cout << st.ways_count() << "\n";
    }
}