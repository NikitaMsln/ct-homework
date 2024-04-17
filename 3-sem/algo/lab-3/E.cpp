#include <bits/stdc++.h>

using namespace std;

vector<size_t> z_func(const string& s) {
    size_t n = s.length();
    vector<size_t> res(n, 0);
    size_t left = 0, right = 0;
    res[0] = 0;
    for (size_t i = 1; i < n; i++) {
        if (i <= right) {
            res[i] = min(right - i + 1, res[i - left]);
        }
        while (i + res[i] < n && s[res[i]] == s[i + res[i]]) {
            res[i]++;
        }
        if (i + res[i] - 1 > right) {
            left = i;
            right = i + res[i] - 1;
        }
    }
    return res;
}

bool is_period(const vector<size_t>& z, size_t v) {
    if (z.size() % v != 0) {
        return false;
    }
    for (size_t i = z.size() / v - 1, p = v; p < z.size(); i--, p += v) {
        if (z[p] != i * v) {
            return false;
        }
    }
    return true;
}

int main() {
    string s;
    cin >> s;
    auto pf = z_func(s);
    for (int i = 1; i <= pf.size(); i++) {
        if (is_period(pf, i)) {
            cout << i << "\n";
            break;
        }
    }
}
