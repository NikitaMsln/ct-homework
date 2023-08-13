#include <bits/stdc++.h>

using namespace std;

class merge_sort_tree {
public:
    merge_sort_tree(size_t size, size_t first, size_t mul, size_t add) {
        size_t data_size = 1;
        for (; data_size < size; data_size *= 2);

        size_t half_data_size = data_size;
        data_size *= 2;

        source_array_ = vector<size_t>(size);
        vector<pair<size_t, size_t>> sorted_array(size);
        for (size_t i = 0; i < size; i++) {
            source_array_[i] = first;
            sorted_array[i] = pair<size_t, size_t>(first, i);
            first = (first * mul + add) % 1'000'000'000;
        }
        sort(sorted_array.begin(), sorted_array.end(), [](const pair<size_t, size_t>& a, const pair<size_t, size_t>& b) {
            return a.first < b.first || (a.first == b.first && a.second < b.second);
        });
        data_ = vector<vector<size_t>>(data_size);

        for (size_t i = half_data_size; i < data_size; i++) {
            if (i < half_data_size + size) {
                data_[i] = vector<size_t>(1, sorted_array[i - half_data_size].second);
            } else {
                data_[i] = vector<size_t>();
            }
        }

        for (size_t i = half_data_size - 1; i > 0; i--) {
            merge(data_[2 * i].begin(), data_[2 * i].end(), data_[2 * i + 1].begin(), data_[2 * i + 1].end(),
                  back_inserter(data_[i]));
        }
    }

    size_t get(size_t left, size_t right, size_t k) const {
        size_t result;
        get(left, right, 1, k, result);
        return source_array_[result];
    }

private:
    size_t get(size_t left, size_t right, size_t index, size_t k, size_t& result) const {
        size_t ll = 0, rl = data_[index].size() + 1;
        while (ll + 1 < rl) {
            size_t m = (ll + rl) / 2;
            if (data_[index][m - 1] < left) {
                ll = m;
            } else {
                rl = m;
            }
        }
        size_t lr = 0, rr = data_[index].size() + 1;
        while (lr + 1 < rr) {
            size_t m = (lr + rr) / 2;
            if (data_[index][m - 1] <= right ) {
                lr = m;
            } else {
                rr = m;
            }
        }
        if (lr < rl) {
            return k;
        } else if (k == 1 && lr == rl) {
            result = data_[index][lr - 1];
            return 0;
        } else if (rr - rl < k) {
            return k - rr + rl;
        } else {
            k = get(left, right, index * 2, k, result);
            if (k == 0) return 0;
            return get(left, right, index * 2 + 1, k, result);
        }
    }

    vector<vector<size_t>> data_;
    vector<size_t> source_array_;
};

size_t
k_stat_sequence_evaluate(const merge_sort_tree &seq, size_t G, size_t x, size_t lx, size_t mx, size_t y, size_t ly,
                         size_t my, size_t k, size_t lk, size_t mk, size_t N) {
    size_t result = 0;
    size_t l = min(x, y), r = max(x, y);
    result += seq.get(l - 1, r - 1, k);
    for (size_t i = 1; i < G; i++) {
        x = ((l - 1) * lx + mx) % N + 1;
        y = ((r - 1) * ly + my) % N + 1;
        l = min(x, y);
        r = max(x, y);
        k = ((k - 1) * lk + mk) % (r - l + 1) + 1;
        result += seq.get(l - 1, r - 1, k);
    }
    return result;
}

int main() {
    size_t N;
    cin >> N;

    size_t a1, l, m;
    cin >> a1 >> l >> m;

    merge_sort_tree seq(N, a1, l, m);

    size_t B;
    cin >> B;
    size_t result = 0;
    while (B--) {
        size_t G, x, lx, mx, y, ly, my, k, lk, mk;
        cin >> G >> x >> lx >> mx >> y >> ly >> my >> k >> lk >> mk;
        result += k_stat_sequence_evaluate(seq, G, x, lx, mx, y, ly, my, k, lk, mk, N);
    }
    cout << result << "\n";
}