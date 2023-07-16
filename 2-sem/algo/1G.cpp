#include <bits/stdc++.h>

using namespace std;

class ways_matrix {
public:
    ways_matrix() {
        for (size_t i = 0; i < 3; i++) {
            for (size_t j = 0; j < 3; j++) {
                matrix[i][j] = 0;
            }
        }
    }

    ways_matrix(const size_t (&other_matrix)[3][3]) noexcept {
        set(other_matrix);
    }

    ways_matrix &operator=(const ways_matrix &other) noexcept {
        if (this == &other) {
            return *this;
        }

        for (size_t i = 0; i < 3; i++) {
            for (size_t j = 0; j < 3; j++) {
                matrix[i][j] = other(i, j);
            }
        }
        return *this;
    }

    void set(const size_t (&other_matrix)[3][3]) noexcept {
        for (size_t i = 0; i < 3; i++) {
            for (size_t j = 0; j < 3; j++) {
                matrix[i][j] = other_matrix[i][j];
            }
        }
    }

    size_t &operator()(size_t i, size_t j) {
        assert(i < 3 && j < 3);
        return matrix[i][j];
    }

    const size_t &operator()(size_t i, size_t j) const {
        assert(i < 3 && j < 3);
        return matrix[i][j];
    }

    friend ways_matrix operator*(const ways_matrix &first, const ways_matrix &second) noexcept;

    ways_matrix &operator*=(const ways_matrix &other) noexcept {
        return (*this = *this * other);
    }

private:
    size_t matrix[3][3];
};

static const ways_matrix E(
        {
                {0, 0, 1},
                {1, 0, 1},
                {0, 1, 1}
        }
);

static const ways_matrix Z(
        {
                {0, 0, 0},
                {1, 0, 0},
                {0, 1, 0}
        }
);

static const size_t MOD = 1'000'000'007;

ways_matrix operator*(const ways_matrix &first, const ways_matrix &second) noexcept {
    ways_matrix result;
    for (size_t i = 0; i < 3; i++) {
        for (size_t j = 0; j < 3; j++) {
            for (size_t x = 0; x < 3; x++) {
                result(i, j) = (result(i, j) + (first(i, x) * second(x, j) % MOD)) % MOD;
            }
        }
    }
    return result;
}

class segment_tree {
public:
    segment_tree(size_t n) {
        data_size_ = 1;
        for (; data_size_ < n; data_size_ *= 2);

        size_t half_data_size_ = data_size_;
        data_size_ *= 2;

        data_ = vector<ways_matrix>(data_size_);
        length_ = vector<size_t>(data_size_);
        bounds_ = vector<size_t[2]>(data_size_);

        for (int i = data_size_ - 1; i >= half_data_size_; i--) {
            if (i < half_data_size_ + n) {
                data_[i] = E;
                length_[i] = 1;
            } else {
                length_[i] = 0;
            }
            bounds_[i][0] = i - half_data_size_;
            bounds_[i][1] = bounds_[i][0] + 1;
        }

        for (int i = half_data_size_ - 1; i > 0; i--) {
            concat(i);
        }
    }

    size_t ways_count() const {
        return data_[1](2, 2);
    }

    void block(size_t index) {
        index += data_size_ / 2;
        if (data_[index](2, 2) != 0) {
            data_[index] = Z;
        } else {
            data_[index] = E;
        }
        while (index > 1) {
            index /= 2;
            concat(index);
        }
    }

private:
    vector<ways_matrix> data_;
    vector<size_t> length_;
    size_t data_size_;
    vector<size_t[2]> bounds_;

    void concat(size_t index) {
        size_t left = index * 2, right = index * 2 + 1;
        if (length_[left] == 0) {
            length_[index] = length_[right];
            data_[index] = data_[right];
        } else if (length_[right] == 0) {
            length_[index] = length_[left];
            data_[index] = data_[left];
        } else {
            length_[index] = length_[left] + length_[right];
            data_[index] = data_[left] * data_[right];
        }
    }
};

int main() {
    auto w2 = E * E;
    auto w3 = E * w2;
    auto w4 = w2 * w2;
    size_t n, m;
    cin >> n >> m;
    segment_tree st(n);
    cout << st.ways_count() << "\n";
    while (m--) {
        size_t index;
        cin >> index;
        st.block(index - 1);
        cout << st.ways_count() << "\n";
    }
}