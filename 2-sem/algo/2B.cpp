#include <bits/stdc++.h>

using namespace std;

class sparse_table {
public:
    explicit sparse_table(const vector<size_t> &data) {
        size_t exp = __lg(data.size());
        if (1 << exp != data.size()) {
            exp++;
        }
        data_ = vector<vector<size_t>>(data.size(), std::vector<size_t>(exp + 1));
        for (size_t i = 0; i < data.size(); i++) {
            data_[i][0] = data[i];
        }
        for (size_t j = 1; j <= exp; j++) {
            for (size_t i = 0; i < data.size(); i++) {
                size_t half_index = i + (1 << (j - 1));
                if (half_index < data.size()) {
                    data_[i][j] = min(data_[i][j - 1], data_[half_index][j - 1]);
                } else {
                    data_[i][j] = data_[i][j - 1];
                }
            }
        }
    }

    size_t min_of_segment(size_t left, size_t right) {
        size_t length = right - left + 1;
        size_t exp = __lg(length);
        if (1 << exp != length) {
            exp++;
        } else {
            return data_[left][exp];
        }
        if (exp == 0) {
            return data_[left][0];
        }
        return min(data_[left][exp - 1], data_[right + 1 - (1 << (exp - 1))][exp - 1]);
    }

private:

    vector<vector<size_t>> data_;
};

int main() {
    size_t n, m, a1, u, v;
    cin >> n >> m >> a1 >> u >> v;

    vector<size_t> a(n);
    a[0] = a1;
    for (size_t i = 1; i < n; i++) {
        a[i] = (23 * a[i - 1] + 21'563) % 16'714'589;
    }
    sparse_table st(a);

    size_t r;
    r = st.min_of_segment(min(u, v) - 1, max(v, u) - 1);
    for (size_t i = 1; i < m; i++) {
        u = (17 * u + 751 + r + 2 * i) % n + 1;
        v = (13 * v + 593 + r + 5 * i) % n + 1;
        r = st.min_of_segment(min(u, v) - 1, max(v, u) - 1);
    }
    cout << u << " " << v << " " << r << "\n";
}