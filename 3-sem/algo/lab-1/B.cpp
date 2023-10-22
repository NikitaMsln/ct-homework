#include <bits/stdc++.h>

using namespace std;

class graph {
private:
    struct edge {
        edge(uint32_t to, uint32_t index) : to(to), index(index) {}

        uint32_t to;
        uint32_t index;
    };

    struct node {
        vector<edge> edges;
    };
public:
    graph(uint32_t n, const vector<pair<uint32_t, uint32_t>>& edges) : nodes_(n) {
        for (size_t i = 0; i < edges.size(); i++) {
            nodes_[edges[i].first - 1].edges.push_back(edge(edges[i].second - 1, i));
            nodes_[edges[i].second - 1].edges.push_back(edge(edges[i].first - 1, i));
        }
    }

    vector<uint32_t> get_bridges() const noexcept {
        vector<uint32_t> ret(nodes_.size(), UINT32_MAX);
        vector<uint32_t> entry(nodes_.size(), UINT32_MAX);
        vector<uint32_t> res;
        uint32_t time = 0;
        for (uint32_t i = 0; i < nodes_.size(); i++) {
            if (ret[i] == UINT32_MAX) {
                dfs(entry, ret, res, time, i, nodes_.size());
            }
        }
        return res;
    }
private:
    void dfs(vector<uint32_t>& entry, vector<uint32_t>& ret, vector<uint32_t>& res, uint32_t& time, uint32_t curr, uint32_t from) const noexcept {
        time++;
        entry[curr] = time;
        ret[curr] = time;
        for (auto next : nodes_[curr].edges) {
            if (next.to == from) {
                continue;
            }
            if (ret[next.to] != UINT32_MAX) {
                ret[curr] = min(ret[curr], entry[next.to]);
            } else {
                dfs(entry, ret, res, time, next.to, curr);
                ret[curr] = min(ret[curr], ret[next.to]);
                if (entry[curr] < ret[next.to]) {
                    res.push_back(next.index);
                }
            }
        }
    }
    vector<node> nodes_;
};

int main() {
    uint32_t n, m;
    cin >> n >> m;
    vector<pair<uint32_t, uint32_t>> edges(m);
    for (size_t i = 0; i < m; i++) {
        cin >> edges[i].first >> edges[i].second;
    }
    graph g(n, edges);
    auto res = g.get_bridges();
    sort(res.begin(), res.end());
    cout << res.size() << "\n";
    for (auto bridge : res) {
        cout << bridge + 1 << "\n";
    }
}
