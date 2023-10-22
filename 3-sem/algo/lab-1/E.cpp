#include <bits/stdc++.h>

using namespace std;

class graph {
private:
    struct edge {
        edge(uint32_t to, uint32_t index) : to(to), index(index) {}

        uint32_t index;
        uint32_t simm;
        uint32_t to;
    };

    struct node {
        vector<edge> edges;
        bool is_common = false;
    };
public:
    graph(uint32_t n, const vector<pair<uint32_t, uint32_t>>& edges) : nodes_(n), edges_count(edges.size()) {
        for (uint32_t i = 0; i < edges.size(); i++) {
            nodes_[edges[i].first - 1].edges.push_back(edge(edges[i].second - 1, i));
            nodes_[edges[i].second - 1].edges.push_back(edge(edges[i].first - 1, i));
            nodes_[edges[i].first - 1].edges.back().simm = nodes_[edges[i].second - 1].edges.size() - 1;
            nodes_[edges[i].second - 1].edges.back().simm = nodes_[edges[i].first - 1].edges.size() - 1;
        }
    }

    vector<uint32_t> get_components() noexcept {
        vector<uint32_t> ret(nodes_.size(), UINT32_MAX);
        vector<uint32_t> entry(nodes_.size(), UINT32_MAX);
        uint32_t time = 0;
        for (uint32_t i = 0; i < nodes_.size(); i++) {
            if (ret[i] == UINT32_MAX) {
                dfs_common(entry, ret, time, i, nodes_.size(), UINT32_MAX);
            }
        }
        vector<bool> visited(nodes_.size(), false);
        uint32_t color = 0;
        vector<uint32_t> res(edges_count, 0);
        for (uint32_t i = 0; i < nodes_.size(); i++) {
            if (!visited[i]) {
                dfs_component(res, color, color, entry, ret, i, visited, nodes_.size(), UINT32_MAX);
            }
        }
        return res;
    }
private:

    void dfs_component(vector<uint32_t>& res, uint32_t curr_color, uint32_t& color, vector<uint32_t>& entry, vector<uint32_t>& ret, uint32_t curr, vector<bool>& visited, uint32_t parent, uint32_t edge) {
        visited[curr] = true;
        for (uint32_t i = 0; i < nodes_[curr].edges.size(); i++) {
            if (nodes_[curr].edges[i].to == parent && nodes_[curr].edges[i].simm == edge) {
                continue;
            }
            if (!visited[nodes_[curr].edges[i].to]) {
                if (ret[nodes_[curr].edges[i].to] >= entry[curr]) {
                    uint32_t new_color = ++color;
                    res[nodes_[curr].edges[i].index] = new_color;
                    dfs_component(res, new_color, color, entry, ret, nodes_[curr].edges[i].to, visited, curr, i);
                } else {
                    res[nodes_[curr].edges[i].index] = curr_color;
                    dfs_component(res, curr_color, color, entry, ret, nodes_[curr].edges[i].to, visited, curr, i);
                }
            } else if (ret[nodes_[curr].edges[i].to] < entry[curr]) {
                res[nodes_[curr].edges[i].index] = curr_color;
            }
        }
    }

    void dfs_common(vector<uint32_t>& entry, vector<uint32_t>& ret, uint32_t& time, uint32_t curr, uint32_t from, uint32_t edge) noexcept {
        time++;
        entry[curr] = time;
        ret[curr] = time;
        uint32_t count = 0;
        bool add = false;
        for (uint32_t i = 0; i < nodes_[curr].edges.size(); i++) {
            if (nodes_[curr].edges[i].to == from && nodes_[curr].edges[i].simm == edge) {
                continue;
            }
            if (ret[nodes_[curr].edges[i].to] != UINT32_MAX) {
                ret[curr] = min(ret[curr], entry[nodes_[curr].edges[i].to]);
            } else {
                dfs_common(entry, ret, time, nodes_[curr].edges[i].to, curr, i);
                count++;
                ret[curr] = min(ret[curr], ret[nodes_[curr].edges[i].to]);
                if (from != nodes_.size() && ret[nodes_[curr].edges[i].to] >= entry[curr]) {
                    add = true;
                }
            }
        }
        if (from == nodes_.size() && count >= 2) {
            add = true;
        }
        nodes_[curr].is_common = add;
    }

    vector<node> nodes_;
    uint32_t edges_count;
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
    for (auto re : res) {
        if (re > max_v) {
            max_v = re;
        }
    }
    cout << max_v << "\n";
    for (auto component : res) {
        cout << component << " ";
    }
    cout << "\n";
}
