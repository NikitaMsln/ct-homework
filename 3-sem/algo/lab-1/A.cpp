#include <bits/stdc++.h>

using namespace std;

bool dfs(list<uint32_t>& res, const vector<vector<uint32_t>>& nodes, vector<bool>& used, vector<bool>& exit, uint32_t curr) {
    used[curr] = true;
    for (uint32_t i : nodes[curr]) {
        if (used[i]) {
            if (exit[i]) {
                continue;
            } else {
                return false;
            }
        } else {
            if (!dfs(res, nodes, used, exit, i)) {
                return false;
            }
        }
    }
    exit[curr] = true;
    res.push_front(curr);
    return true;
}

int main() {
    uint32_t n, m;
    cin >> n >> m;
    vector<vector<uint32_t>> nodes(n);
    for (uint32_t i = 0; i < m; i++) {
        uint32_t f, t;
        cin >> f >> t;
        nodes[f - 1].push_back(t - 1);
    }
    vector<bool> used(n, false);
    vector<bool> exit(n, false);
    list<uint32_t> res;
    uint32_t i = 0;
    while (i != n) {
        if (used[i]) {
            i++;
        } else {
            if (!dfs(res, nodes, used, exit, i)) {
                cout << "-1\n";
                return 0;
            }
        }
    }
    for (uint32_t node : res) {
        cout << node + 1 << " ";
    }
    cout << "\n";
}