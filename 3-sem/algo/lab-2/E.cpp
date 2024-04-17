#include <bits/stdc++.h>

using namespace std;

vector<int64_t> solve(const vector<set<pair<int64_t, int>>>& edges, int s) {
    vector<int64_t> d(edges.size(), INT64_MAX);
    d[s] = 0;
    vector<bool> visited(edges.size(), false);
    visited[s] = true;
    for (int i = 0; i < edges.size() - 1; i++) {
        for (int f = 0; f < edges.size(); f++) {
            if (!visited[f]) {
                continue;
            }
            for (auto& t : edges[f]) {
                visited[t.second] = true;
                if (d[t.second] > d[f] + t.first) {
                    d[t.second] = d[f] + t.first;
                }
            }
        }
    }
    for (int i = 0; i < edges.size() - 1; i++) {
        for (int f = 0; f < edges.size(); f++) {
            if (!visited[f]) {
                continue;
            }
            for (auto& t : edges[f]) {
                if (d[f] == INT64_MIN || d[t.second] > d[f] + t.first) {
                    d[t.second] = INT64_MIN;
                }
            }
        }
    }
    return d;
}

int main() {
    int n, m, s;
    cin >> n >> m >> s;
    vector<set<pair<int64_t, int>>> edges(n);
    for (int i = 0; i < m; i++) {
        int a, b;
        int64_t c;
        cin >> a >> b >> c;
        edges[a - 1].insert({c, b - 1});
    }
    auto a = solve(edges, s - 1);
    for (int i = 0; i < n; i++) {
        if (a[i] == INT64_MAX) {
            cout << "*\n";
        } else if (a[i] == INT64_MIN) {
            cout << "-\n";
        } else {
            cout << a[i] << "\n";
        }
    }
}