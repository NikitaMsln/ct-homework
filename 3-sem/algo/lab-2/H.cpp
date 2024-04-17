#include <bits/stdc++.h>
 
using namespace std;
 
static const bool WIN = true;
static const int LOS = false;
 
void dfs(const vector<vector<int>>& nodes, vector<bool>& st, vector<bool>& visited, int curr) {
    visited[curr] = true;
    if (nodes[curr].size() == 0) {
        st[curr] = LOS;
        return;
    }
    bool has_los = false;
    for (auto next : nodes[curr]) {
        if (!visited[next]) {
            dfs(nodes, st, visited, next);
        }
        if (st[next] == LOS) {
            has_los = true;
        }
    }
    st[curr] = (has_los)? WIN : LOS;
}
 
int main() {
    int n, m, s;
    cin >> n >> m >> s;
    s--;
    vector<vector<int>> nodes(n);
    for (int i = 0; i < m; i++) {
        int u, v;
        cin >> u >> v;
        nodes[u - 1].push_back(v - 1);
    }
    vector<bool> visited(n, false);
    vector<bool> st(n, false);
    dfs(nodes, st, visited, s);
    if (st[s] == WIN) {
        cout << "First player wins\n";
    } else {
        cout << "Second player wins\n";
    }
}