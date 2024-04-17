#include <bits/stdc++.h>

using namespace std;
 
inline const int WIN = 1;
inline const int DRAW = 0;
inline const int LOS = -1;

void dfs(const vector<set<int>>& inv_edges, const vector<set<int>>& edges, vector<int>& res, vector<int>& fr, vector<bool>& visited, int curr, int val) {
    if (visited[curr] || (fr[curr] < edges[curr].size() && val == LOS)) {
        return;
    }
    visited[curr] = true;
    res[curr] = val;
    for (auto next : inv_edges[curr]) {
        fr[next]++;
        dfs(inv_edges, edges, res, fr, visited, next, -val);
    }
}

void sol() {
    int n, m;
    cin >> n >> m;
    vector<set<int>> edges(n), inv_edges(n);
    for (int i = 0; i < m; i++) {
        int f, t;
        cin >> f >> t;
        edges[f - 1].insert(t - 1);
        inv_edges[t - 1].insert(f - 1);
    }
    vector<int> res(n, DRAW);
    vector<int> fr(n, 0);
    vector<bool> visited(n, false);
    set<int> leafs;
    for (int i = 0; i < n; i++) {
        if (edges[i].empty()) {
            leafs.insert(i);
        }
    }
    for (auto it = leafs.begin(); it != leafs.end(); it++) {
        dfs(inv_edges, edges, res, fr, visited, *it, LOS);
    }
    for (int i : res) {
        switch (i)
        {
        case LOS:
            cout << "SECOND\n";
            break;
        case WIN:
            cout << "FIRST\n";
            break;
        case DRAW:
            cout << "DRAW\n";
            break;
        default:
            break;
        }
    }
    cout << "\n";
}

int main() {
    int t;
    cin >> t;
    while (t--) sol();
}