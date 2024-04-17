#include <bits/stdc++.h>

using namespace std;

vector<int64_t> calc_lcp(const string &val, const vector<int64_t> &c, const vector<int64_t> &p) {
    int64_t n = val.size();
    int64_t current_lcp = 0;
    vector<int64_t> lcp(n - 1);
    for (int64_t i = 0; i < n; i++) {
        if (c[i] == n - 1)
            continue;
        int64_t nxt = p[c[i] + 1];
        while (max(i, nxt) + current_lcp < n && val[i + current_lcp] == val[nxt + current_lcp])
            current_lcp++;
        lcp[c[i]] = current_lcp;
        current_lcp = max((int64_t)0, current_lcp - 1);
    }
    return lcp;
}

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

    for (int l = 1; class_count < n; l++) {
        vector<vector<int64_t>> a(class_count);
        vector<int64_t> cp(n);
        int d = (1 << l) / 2;
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
    
    res.second = calc_lcp(s, c, p);
    return res;
}

int main() {
    string s;
    cin >> s;
    s.push_back('\0');
    auto p = suff_array(s);
    for (int i = 1; i < p.first.size(); i++) {
        cout << (p.first)[i] + 1 << " ";
    }
    cout << "\n";
    for (int i = 1; i < p.second.size(); i++) {
        cout << (p.second)[i] << " ";
    }
    cout << "\n";
}