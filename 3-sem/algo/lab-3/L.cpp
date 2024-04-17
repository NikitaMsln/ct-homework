#include <bits/stdc++.h>

using namespace std;

pair<vector<int64_t>, vector<int64_t>> suff_array(const string& s) {
    int64_t n = s.size();
    int64_t counter = 0;
    pair<vector<int64_t>, vector<int64_t>> res(vector<int64_t>(n), vector<int64_t>(n - 1, 0));
    vector<int64_t> &p = res.first;
    vector<int64_t> c(n);
    vector<vector<size_t>> cnt(256);

    int64_t class_count = 0;

    for (int i = 0; i < n; i++) cnt[s[i]].push_back(i);
    
    for (auto &x : cnt) {
        if (!x.empty()) {
            for (int u : x) {
                c[u] = class_count;
                p[counter++] = u;
            }
            class_count++;
        }
    }

    for (int d = 1; d < n; d *= 2) {
        vector<vector<int64_t>> a(class_count);
        vector<int64_t> cp(n);
        int _cls = counter = -1;
        
        for (int i = 0; i < n; i++) {
            int k = (p[i] - d + n) % n;
            a[c[k]].push_back(k);
        }
        
        for (int i = 0; i < class_count; i++) {
            for (size_t j = 0; j < a[i].size(); j++) {
                if (j == 0 || c[(a[i][j] + d) % n] != c[(a[i][j-1] + d) % n]) {
                    _cls++;
                }
                cp[a[i][j]] = _cls;
                p[++counter] = a[i][j];
            }
        }
        
        c = cp;
        class_count = _cls + 1;
    }

    
    res.second = c;
    return res;
}

void printn(const string& s, int start) {
    string out;
    out.reserve(s.size());
    for (int i = 0; i < s.size(); i++) {
        out.push_back(s[(start + i) % s.size()]);
    }
    cout << out << "\n";
}

int main() {
    string s;
    cin >> s;
    int k;
    cin >> k;
    auto p = suff_array(s);
    for (int i = 0; i < p.first.size(); i++) {
        if (p.second[i] == k - 1) {
            printn(s, i);
            return 0;
        }
    }
    cout << "IMPOSSIBLE\n";
}