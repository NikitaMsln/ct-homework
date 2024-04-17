#include <bits/stdc++.h>

using namespace std;

set<int> set_st(int n) {
    set<int> res;
    for (int i = 0; i <= n; i++) {
        res.insert(i);
    }
    return res;
}

void dfs(const vector<vector<int>>& inv_edges, const vector<vector<int>>& edges, vector<int>& res, vector<int>& fr, int curr) {
    if (fr[curr] < edges[curr].size()) {
        return;
    }
    auto range = set_st(edges[curr].size());
    for (auto i : edges[curr]) {
        if (auto it = range.find(res[i]); it != range.end()) {
            range.erase(it);
        }
    }
    res[curr] = *range.lower_bound(0);
    for (auto i : inv_edges[curr]) {
        fr[i]++;
        dfs(inv_edges, edges, res, fr, i);
    }
}

vector<int> grandi_func(const vector<vector<int>>& inv_edges, const vector<vector<int>>& edges) {
    vector<int> res(edges.size(), -1);
    vector<int> fr(edges.size(), 0);
    for (int i = 0; i < edges.size(); i++) {
        if (edges[i].size() == 0) {
            dfs(inv_edges, edges, res, fr, i);
        }
    }
    return res;
}

int main() {
    int n, m;
    cin >> n >> m;
    vector<vector<int>> edges(n);
    vector<vector<int>> inv_edges(n);
    for (int i = 0; i < m; i++) {
        int f, t;
        cin >> f >> t;
        edges[f - 1].push_back(t - 1);
        inv_edges[t - 1].push_back(f - 1);
    }
    auto ans = grandi_func(inv_edges, edges);
    for (auto i : ans) {
        cout << i << "\n";
    }
}