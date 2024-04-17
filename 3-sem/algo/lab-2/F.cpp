#include <bits/stdc++.h>

using namespace std;

struct comp {
    bool operator()(const pair<int, int64_t>& a, const pair<int, int64_t>& b) const {
        return a.second > b.second;
    }
};

vector<int64_t> dejkst(const vector<set<pair<int, int64_t>>>& edges, int s, int p1, int p2) {
    vector<int64_t> res(edges.size(), INT64_MIN / 2);
    res[s] = 0;
    priority_queue<pair<int, int64_t>, vector<pair<int, int64_t>>, comp> q(edges[s].begin(), edges[s].end());
    vector<bool> used(edges.size(), false);
    used[s] = true;
    int k = 1;
    int l = 0;
    while (!q.empty() && k < edges.size() && l < 2) {
        auto cur = q.top(); q.pop();
        if (!used[cur.first]) {
            res[cur.first] = cur.second;
            k++;
            if (cur.first == p1 || cur.first == p2) {
                l++;
            }
            used[cur.first] = true;
            for (auto& p : edges[cur.first]) {
                q.push({p.first, p.second + cur.second});
            }
        }
    }
    return res;
}

int64_t min_pos(int64_t a, int64_t b, int64_t c, int64_t d, int64_t e, int64_t f) {
    if (a < 0 || (a > d && d > 0)) {
        a = d;
    }
    if (b < 0 || (b > e && e > 0)) {
        b = e;
    }
    if (c < 0 || (c > f && f > 0)) {
        c = f;
    }
    if (a < 0 || (a > c && c > 0)) {
        a = c;
    }
    if (a < 0 || (a > b && b > 0)) {
        a = b;
    }
    return a;
}

int main() {
    int n, m;
    cin >> n >> m;
    vector<set<pair<int, int64_t>>> edges(n);
    for (int i = 0; i < m; i++) {
        int u, v, w;
        cin >> u >> v >> w;
        edges[u - 1].insert({v - 1, w});
        edges[v - 1].insert({u - 1, w});
    }
    int a, b, c;
    cin >> a >> b >> c;
    auto ra = dejkst(edges, a - 1, b - 1, c - 1);
    auto rb = dejkst(edges, b - 1, a - 1, c - 1);
    auto rc = dejkst(edges, c - 1, a - 1, b - 1);
    int64_t res = min_pos(ra[b - 1] + rb[c - 1], ra[c - 1] + rc[b - 1], rc[a - 1] + ra[b - 1], rc[b - 1] + rb[a - 1], rb[c - 1] + rc[a - 1], rb[a - 1] + ra[c - 1]);
    if (res < 0) cout << "-1\n";
    else cout << res << "\n";
}