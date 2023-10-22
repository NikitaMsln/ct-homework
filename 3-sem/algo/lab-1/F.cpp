#include <bits/stdc++.h>

using namespace std;

class graph {
public:
    graph(uint32_t n, const vector<pair<uint32_t, uint32_t>>& edges) : edges_(n), inverted_edges_(n) {
        for (size_t i = 0; i < edges.size(); i++) {
            edges_[edges[i].first - 1].push_back(edges[i].second - 1);
            inverted_edges_[edges[i].second - 1].push_back(edges[i].first - 1);
        }
    }

    uint32_t get_cond() {
        vector<bool> visited(edges_.size(), false);
        vector<uint32_t> order;
        for (uint32_t i = 0; i < edges_.size(); i++) {
            if (!visited[i]) {
                dfs1(visited, order, i);
            }
        }
        uint32_t comp = 1;
        vector<uint32_t> component(edges_.size(), 0);
        for (uint32_t i = order.size(); i > 0; i--) {
            if (component[order[i - 1]] == 0) {
                dfs2(component, comp, order[i - 1]);
                comp++;
            }
        }
        vector<set<uint32_t>> res(comp - 1);
        visited = std::move(vector<bool>(edges_.size(), false));
        for (uint32_t i = 0; i < edges_.size(); i++) {
            if (!visited[i]) {
                dfs3(visited, component, i, res);
            }
        }
        uint32_t result = 0;
        for (auto& i : res) {
            result += i.size();
        }
        return result;
    }

private:
    vector<vector<uint32_t>> edges_;
    vector<vector<uint32_t>> inverted_edges_;

    void dfs1(vector<bool>& visited, vector<uint32_t>& order, uint32_t curr) {
        visited[curr] = true;
        for (uint32_t i = 0; i < edges_[curr].size(); i++) {
            if (!visited[edges_[curr][i]]) {
                dfs1(visited, order, edges_[curr][i]);
            }
        }
        order.push_back(curr);
    }

    void dfs2(vector<uint32_t>& component, uint32_t curr_comp, uint32_t curr) {
        component[curr] = curr_comp;
        for (uint32_t i = 0; i < inverted_edges_[curr].size(); i++) {
            if (component[inverted_edges_[curr][i]] == 0) {
                dfs2(component, curr_comp, inverted_edges_[curr][i]);
            }
        }
    }

    void dfs3(vector<bool>& visited, vector<uint32_t>& component, uint32_t curr, vector<set<uint32_t>>& res) {
        visited[curr] = true;
        for (uint32_t i = 0; i < edges_[curr].size(); i++) {
            if (component[curr] != component[edges_[curr][i]]) {
                if (res[component[curr] - 1].find(component[edges_[curr][i]] - 1) == res[component[curr] - 1].end()) {
                    res[component[curr] - 1].insert(component[edges_[curr][i]] - 1);
                }
            }
            if (!visited[edges_[curr][i]]) {
                dfs3(visited, component, edges_[curr][i], res);
            }
        }
    }

    graph(uint32_t n) : edges_(n), inverted_edges_(n) {}
};

int main() {
    uint32_t n, m;
    cin >> n >> m;
    vector<pair<uint32_t, uint32_t>> edges(m);
    for (size_t i = 0; i < m; i++) {
        cin >> edges[i].first >> edges[i].second;
    }
    graph g(n, edges);
    cout << g.get_cond() << "\n";
}
