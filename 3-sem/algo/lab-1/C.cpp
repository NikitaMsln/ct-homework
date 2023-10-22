#include <bits/stdc++.h>

using namespace std;

class graph {
private:
    struct edge {
        edge(uint32_t to) : to(to) {}

        uint32_t to;
    };

    struct node {
        vector<edge> edges;
    };
public:
    graph(uint32_t n, const vector<pair<uint32_t, uint32_t>>& edges) : nodes_(n) {
        for (const auto & i : edges) {
            nodes_[i.first - 1].edges.push_back(edge(i.second - 1));
            nodes_[i.second - 1].edges.push_back(edge(i.first - 1));
        }
    }

    vector<uint32_t> get_nodes() const noexcept {
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
        uint32_t count = 0;
        bool add = false;
        for (auto next : nodes_[curr].edges) {
            if (next.to == from) {
                continue;
            }
            if (ret[next.to] != UINT32_MAX) {
                ret[curr] = min(ret[curr], entry[next.to]);
            } else {
                dfs(entry, ret, res, time, next.to, curr);
                count++;
                ret[curr] = min(ret[curr], ret[next.to]);
                if (from != nodes_.size() && ret[next.to] >= entry[curr]) {
                    add = true;
                }
            }
        }
        if (from == nodes_.size() && count >= 2) {
            add = true;
        }
        if (add) {
            res.push_back(curr);
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
    auto res = g.get_nodes();
    sort(res.begin(), res.end());
    cout << res.size() << "\n";
    for (auto bridge : res) {
        cout << bridge + 1 << " ";
    }
    cout << "\n";
}
