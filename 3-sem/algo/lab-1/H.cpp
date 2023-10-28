#include <bits/stdc++.h>

using namespace std;

class graph {
private:
    struct edge {
        edge() noexcept = default;

        edge(uint32_t from, uint32_t to, uint32_t value) noexcept: from(from), to(to), value(value) {}

        friend bool operator<(const edge &a, const edge &b) noexcept {
            return a.value < b.value;
        }

        uint32_t from = 0;
        uint32_t to = 0;
        uint32_t value = 0;
    };

public:
    explicit graph(vector<vector<uint32_t>> &&way_matrix) : way_matrix_(std::move(way_matrix)) {}

    uint32_t get_min_edge_value(uint32_t max_value) noexcept {
        uint32_t l = 0, r = max_value + 1;
        while (l < r - 1) {
            uint32_t m = (l + r) / 2;
            bool c = is_comp(m);
            if (c) {
                r = m;
            } else {
                l = m;
            }
        }
        return l;
    }

private:
    vector<vector<uint32_t>> way_matrix_;

    bool is_comp(uint32_t max) {
        vector<bool> visited(way_matrix_.size(), false);
        vector<uint32_t> order;
        dfs1(visited, order, 0, max);
        for (uint32_t i = 1; i < way_matrix_.size(); i++) {
            if (!visited[i]) {
                return false;
            }
        }
        uint32_t comp = 1;
        vector<uint32_t> component(way_matrix_.size(), 0);
        dfs2(component, comp, order.front(), max);
        for (uint32_t i = order.size(); i > 0; i--) {
            if (component[order[i - 1]] == 0) {
                return false;
            }
        }
        return true;
    }

    void dfs1(vector<bool>& visited, vector<uint32_t>& order, uint32_t curr, uint32_t max_v) {
        visited[curr] = true;
        for (uint32_t i = 0; i < way_matrix_.size(); i++) {
            if (!visited[i] && way_matrix_[i][curr] < max_v) {
                dfs1(visited, order, i, max_v);
            }
        }
        order.push_back(curr);
    }

    void dfs2(vector<uint32_t>& component, uint32_t curr_comp, uint32_t curr, uint32_t max_v) {
        component[curr] = curr_comp;
        for (uint32_t i = 0; i < way_matrix_.size(); i++) {
            if (component[i] == 0 && way_matrix_[curr][i] < max_v) {
                dfs2(component, curr_comp, i, max_v);
            }
        }
    }
};

int main() {
    uint32_t n;
    cin >> n;
    uint32_t max_v = 0;
    vector<vector<uint32_t>> way_matrix(n, vector<uint32_t>(n));
    for (uint32_t i = 0; i < n; i++) {
        for (uint32_t j = 0; j < n; j++) {
            cin >> way_matrix[i][j];
            if (way_matrix[i][j] > max_v) {
                max_v = way_matrix[i][j];
            }
        }
    }
    if (max_v == 0) {
        cout << "0\n";
        return 0;
    }
    graph g(std::move(way_matrix));
    cout << g.get_min_edge_value(max_v) << "\n";
}