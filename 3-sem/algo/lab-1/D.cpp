#include <bits/stdc++.h>

using namespace std;

class graph {
private:
    struct edge {
        edge(uint32_t to) : to(to){}

        bool is_bridge = false;
        uint32_t simm = UINT32_MAX;
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
            nodes_[i.first - 1].edges.back().simm = nodes_[i.second - 1].edges.size() - 1;
            nodes_[i.second - 1].edges.back().simm = nodes_[i.first - 1].edges.size() - 1;
        }
    }

    vector<uint32_t> get_components() noexcept {
        find_bridges();
        vector<uint32_t> res(nodes_.size(), UINT32_MAX);
        uint32_t index_count = 0;
        for (uint32_t i = 0; i < nodes_.size(); i++) {
            if (res[i] == UINT32_MAX) {
                index_count++;
                dfs_c(res, index_count, index_count, i);
            }
        }
        return std::move(res);
    }

private:

    void dfs_c(vector<uint32_t>& res, uint32_t curr_index, uint32_t& comp_index, uint32_t curr) const noexcept {
        res[curr] = curr_index;
        for (const auto& next : nodes_[curr].edges) {
            if (res[next.to] == UINT32_MAX) {
                if (next.is_bridge) {
                    comp_index++;
                    dfs_c(res, comp_index, comp_index, next.to);
                } else {
                    dfs_c(res, curr_index, comp_index, next.to);
                }
            }
        }
    }

    void find_bridges() noexcept {
        vector<uint32_t> ret(nodes_.size(), UINT32_MAX);
        vector<uint32_t> entry(nodes_.size(), UINT32_MAX);
        uint32_t time = 0;
        for (uint32_t i = 0; i < nodes_.size(); i++) {
            if (ret[i] == UINT32_MAX) {
                dfs_b(entry, ret, time, i, nodes_.size(), UINT32_MAX);
            }
        }
    }

    void dfs_b(vector<uint32_t>& entry, vector<uint32_t>& ret, uint32_t& time, uint32_t curr, uint32_t from, uint32_t edge) noexcept {
        time++;
        entry[curr] = time;
        ret[curr] = time;
        for (uint32_t i = 0; i < nodes_[curr].edges.size(); i++) {
            if (nodes_[curr].edges[i].to == from && i == edge) {
                continue;
            }
            if (ret[nodes_[curr].edges[i].to] != UINT32_MAX) {
                ret[curr] = min(ret[curr], entry[nodes_[curr].edges[i].to]);
            } else {
                dfs_b(entry, ret, time, nodes_[curr].edges[i].to, curr, nodes_[curr].edges[i].simm);
                ret[curr] = min(ret[curr], ret[nodes_[curr].edges[i].to]);
                if (entry[curr] < ret[nodes_[curr].edges[i].to]) {
                    nodes_[curr].edges[i].is_bridge = true;
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
    auto res = g.get_components();
    uint32_t max_v = 0;
    for (size_t i = 0; i < n; i++) {
        if (res[i] > max_v) {
            max_v = res[i];
        }
    }
    cout << max_v << "\n";
    for (auto component : res) {
        cout << component << " ";
    }
    cout << "\n";
}