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

int main() {
    string p, t;
    cin >> p;
    cin >> t;
    size_t m = p.length();
    t = p + "@" + t;
    size_t n = t.length();
    auto pf = z_func(t);
    vector<size_t> subs;
    for (int i = m + 1; i < pf.size(); i++) {
        if (pf[i] == m) {
            subs.push_back(i - m);
        }
    }
    cout << subs.size() << "\n";
    for (auto i : subs) {
        cout << i << " ";
    }
    cout << "\n";
}
