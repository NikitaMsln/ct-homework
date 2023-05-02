#include <bits/stdc++.h>

using namespace std;

class SegmentTree {
private:
    vector<long long[7]> data;
    vector<int[2]> bounds;
    vector<long long[5]> exps;
    int size;
    int data_size;

    static void set_values(long long * dst, const long long * src1, const long long * src2) {
        dst[0] = (src1[0] < src2[0])? src1[0] : src2[0];
        dst[1] = (src1[1] > src2[1])? src1[1] : src2[1];
        for (int i = 2; i < 7; i++) dst[i] = src1[i] + src2[i];
    }

    void get_segment(int left, int right, int index, long long* result) const {
        if (bounds[index][1] <= left || bounds[index][0] >= right) {
            result[0] = INT32_MAX;
            result[1] = INT32_MIN;
            for (int i = 2; i < 7; i++) result[i] = 0;
        } else if (bounds[index][0] >= left && bounds[index][1] <= right) {
            for (int i = 0; i < 7; i++) result[i] = data[index][i];
        } else {
            long long src1[7], src2[7];
            get_segment(left, right, index * 2, src1);
            get_segment(left, right, index * 2 + 1, src2);
            set_values(result, src1, src2);
        }
    }
public:
    explicit SegmentTree(const vector<int>& array) {
        data_size = 1;
        for (; data_size < array.size(); data_size *= 2);

        int half_data_size = data_size;
        data_size *= 2;

        size = array.size();
        data = vector<long long[7]>(data_size);
        bounds = vector<int[2]>(data_size);
        exps = vector<long long[5]>(size + 1);
        for (int i = 0; i < 5; i++) exps[0][i] = 0;

        for (int i = half_data_size, j = 0; i < data_size; i++, j++) {
            if (i >= half_data_size + size) {
                data[i][0] = INT32_MAX;
                data[i][1] = INT32_MIN;
                for (int l = 2; l < 7; l++) data[i][l] = 0;
            } else {
                data[i][0] = data[i][1] = array[j];
                for (int l = 0, k = j + 1, p = array[j]; l < 5; l++, k *= (j + 1), p *= array[j]) {
                    exps[j + 1][l] = exps[j][l] + k;
                    data[i][l + 2] = p;
                }
            }
            bounds[i][0] = i - half_data_size;
            bounds[i][1] = bounds[i][0] + 1;
        }

        for (int i = half_data_size - 1; i > 0; i--) {
            set_values(data[i], data[i * 2], data[i * 2 + 1]);
            bounds[i][0] = bounds[i * 2][0];
            bounds[i][1] = bounds[i * 2 + 1][1];
        }
    }

    void set(int index, int value) {
        if (index < 0 || index >= size) {
            throw exception();
        }
        index += data_size / 2;
        data[index][0] = data[index][1] = value;
        for (int l = 2, p = value; l < 7; l++, p *= value) {
            data[index][l] = p;
        }
        while (index > 1) {
            index =  index / 2;
            set_values(data[index], data[index * 2], data[index * 2 + 1]);
        }
    }

    bool check_perm(int left, int right) const {
        long long info[7];
        get_segment(left, right, 1, info);
        int n = right - left;
        bool eq = true;
        for (int i = 0; i < 5 && eq; i++) {
            eq = eq && info[i + 2] == exps[n][i];
        }
        return info[0] == 1 && info[1] == n && eq;
    }
};

int main() {
    fstream in, out;
    in.open("permutation.in", ios_base::in);
    out.open("permutation.out", ios_base::out);

    int n;
    in >> n;
    vector<int> a(n);
    for (int i = 0; i < n; i++) in >> a[i];

    SegmentTree tree(a);

    int m;
    in >> m;
    for (int i = 0; i < m; i++) {
        int t, x, y;
        in >> t >> x >> y;
        if (t == 1) {
            tree.set(x - 1, y);
        } else {
            if (tree.check_perm( x - 1, y)) {
                out << "YES\n";
            } else {
                out << "NO\n";
            }
        }
    }

    in.close();
    out.close();
}
