#include <bits/stdc++.h>

using namespace std;

class graph {
public:
    graph(uint32_t n, const vector<pair<uint32_t, uint32_t>>& edges) : edges_(n), inverted_edges_(n) {
        for (size_t i = 0; i < edges.size(); i++) {
            edges_[edges[i].first].push_back(edges[i].second);
            inverted_edges_[edges[i].second].push_back(edges[i].first);
        }
    }

    list<uint32_t> topsort() const {
        vector<bool> used(edges_.size(), false);
        vector<bool> exit(edges_.size(), false);
        list<uint32_t> res;
        for (uint32_t i = 0; i < edges_.size(); i++) {
            if (!used[i]) {
                dfs4(res, used, exit, i);
            }
        }
        return res;
    }

    pair<graph, vector<uint32_t>> get_condens() {
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
        graph g_res(edges_.size());

        for (uint32_t i = 0; i < res.size(); i++) {
            for (auto v : res[i]) {
                g_res.edges_[i].push_back(v);
                g_res.inverted_edges_[v].push_back(i);
            }
        }
        return pair(g_res, component);
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


    void dfs4(list<uint32_t>& res, vector<bool>& used, vector<bool>& exit, uint32_t curr) const {
        used[curr] = true;
        for (uint32_t i : edges_[curr]) {
            if (used[i]) {
                if (exit[i]) {
                    continue;
                } else {
                    throw runtime_error("Unaccepted to topsort");
                }
            } else {
                dfs4(res, used, exit, i);
            }
        }
        exit[curr] = true;
        res.push_front(curr);
    }

    graph(uint32_t n) : edges_(n), inverted_edges_(n) {}
};

bool is_more_far(uint32_t i, uint32_t j, list<uint32_t>& container) {
    for (auto k : container) {
        if (k == i) {
            return false;
        } else if (k == j) {
            return true;
        }
    }
}

int main() {
    map<string, uint32_t> name_to_i;
    uint32_t n, m;
    cin >> n >> m;
    vector<string> i_to_name(n);
    for (uint32_t i = 0; i < n; i++) {
        cin >> i_to_name[i];
        name_to_i[i_to_name[i]] = i;
    }
    vector<pair<uint32_t, uint32_t>> edges;
    for (uint32_t i = 0; i < m; i++) {
        char fv, tv;
        string f;
        string t;
        cin >> fv;
        cin >> f;
        cin >> t;
        cin >> tv;
        cin >> t;
        uint32_t fi = 2 * name_to_i[f];
        uint32_t fi_s = fi;
        if (fv == '-') {
            fi++;
        } else {
            fi_s++;
        }
        uint32_t ti = 2 * name_to_i[t];
        uint32_t ti_s = ti;
        if (tv == '-') {
            ti++;
        } else {
            ti_s++;
        }
        edges.push_back(pair(fi, ti));
        edges.push_back(pair(ti_s, fi_s));
    }
    graph g(n * 2, edges);
    auto r = g.get_condens();
    auto c = r.first.topsort();
    vector<uint32_t> res;
    for (uint32_t i = 0; i < 2 * n; i += 2) {
        if (r.second[i] != r.second[i + 1]) {
            if (is_more_far(r.second[i] - 1, r.second[i + 1] - 1, c)) {
                res.push_back(i / 2);
            }
        } else {
            cout << "-1\n";
            return 0;
        }
    }
    cout << res.size() << "\n";
    for (auto i : res) {
        cout << i_to_name[i] << "\n";
    }
}