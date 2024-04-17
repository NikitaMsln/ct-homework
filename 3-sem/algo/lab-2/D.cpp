#include <bits/stdc++.h>
 
using namespace std;
 
static const int64_t INF = INT64_MAX;
 
int main() {
    int64_t n, m, k, s;
    cin >> n >> m >> k >> s;
    s--;
    vector<vector<pair<int64_t, int64_t>>> matrix(n);
    for (int i = 0; i < m; i++) {
        int u, v, w;
        cin >> u >> v >> w;
        matrix[u - 1].emplace_back(v - 1, w);
    }
 
    vector<int64_t> d(n, INF);
    d[s] = 0;
    while (k--) {
        vector<int64_t> nd(n, INF);
        for (int i = 0; i < n; i++) {
            if (d[i] == INF) continue;
 
            for (auto next : matrix[i]) {
                nd[next.first] = min(nd[next.first], d[i] + next.second);
            }
        }
        d = std::move(nd);
    }
    for (int i = 0; i < n; i++) {
        if (d[i] == INF) {
            cout << "-1\n";
        } else {
            cout << d[i] << "\n";
        }
    }
}