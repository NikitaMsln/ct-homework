#include <bits/stdc++.h>
 
using namespace std;
 
struct comp {
    bool operator()(const pair<uint32_t, uint32_t> &a, const pair<uint32_t, uint32_t> &b) const {
        return a.first > b.first || (a.first == b.first && a.second > b.second);
    }
};
 
int main() {
    uint32_t n, m;
    cin >> n >> m;
 
    vector<vector<pair<uint32_t, uint32_t>>> nodes(n);
    for (int i = 0; i < m; i++) {
        uint32_t u, v, w;
        cin >> u >> v >> w;
        nodes[u - 1].push_back({w, v - 1});
        nodes[v - 1].push_back({w, u - 1});
    }
    vector<uint64_t> d(n, UINT32_MAX);
    d[0] = 0;
 
    priority_queue<pair<uint32_t, uint32_t>, vector<pair<uint32_t, uint32_t>>, comp> q;
    q.push({0, 0});
 
    while (!q.empty()) {
        uint32_t w = q.top().first;
        uint32_t v = q.top().second;
        q.pop();
        if (w > d[v]) continue;
        for (int i = 0; i < nodes[v].size(); i++) {
            uint32_t to = nodes[v][i].second;
            uint32_t length = nodes[v][i].first;
            if (d[to] > d[v] + length) {
                d[to] = d[v] + length;
                q.push({d[to], to});
            }
        }
    }
    for (int i = 0; i < n; i++) {
        cout << d[i] << " ";
    }
    cout << "\n";
}