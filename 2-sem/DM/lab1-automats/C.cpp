#include <bits/stdc++.h>

#define MOD ((long long)(1e9 + 7))

using namespace std;

class automat {
private:
    struct node {
        vector<int> reversed_edges = vector<int>();
        vector<long long> count = vector<long long>();
        vector<int> edges = vector<int>();
    };
    vector<node> nodes;
    vector<int> accepted_nodes;
public:
    automat(int n) : nodes(vector<node>(n)), accepted_nodes(vector<int>()) {}

    void accept_node(int i) {
        accepted_nodes.push_back(i);
    }

    void add_edge(int from, int to) {
        nodes[from].edges.push_back(to);
        for (size_t i = 0; i < nodes[to].reversed_edges.size(); i++) {
            if (nodes[to].reversed_edges[i] == from) {
                nodes[to].count[i]++;
                return;
            }
        }
        nodes[to].reversed_edges.push_back(from);
        nodes[to].count.push_back(1);
    }

    void dfs(vector<bool>& find, int node) {
        if (find[node]) {
            return;
        }
        find[node] = true;
        for (int last_node : nodes[node].reversed_edges) {
            dfs(find, last_node);
        }
    }

    bool has_cycle(vector<int>& cycles, vector<bool>& find, vector<int>& last_nodes, int node) {
        cycles[node] = 1;
        for (size_t i = 0; i < nodes[node].edges.size(); i++) {
            int cur = nodes[node].edges[i];
            if (cycles[cur] == 0) {
                if (has_cycle(cycles, find, last_nodes, cur)) return true;
            } else if (find[cur] && cycles[cur] == 1) {
                return true;
            }
        }
        cycles[node] = 2;
        last_nodes.push_back(node);
        return false;
    }

    long long count() {
        vector<int> cycles(nodes.size(), 0), last_nodes;
        vector<bool> find(nodes.size(), false);
        long long result = -1;
        for (int i : accepted_nodes)
            dfs(find, i);
        if (!has_cycle(cycles, find, last_nodes, 0)) {
            reverse(last_nodes.begin(), last_nodes.end());
            result = 0;
            vector<long long> paths(nodes.size());
            paths[0] = 1;
            for (int cur : last_nodes) {
                for (size_t last_node = 0; last_node < nodes[cur].reversed_edges.size(); last_node++) {
                    paths[cur] = (paths[cur] + paths[nodes[cur].reversed_edges[last_node]] * nodes[cur].count[last_node]) % MOD;
                }
            }
            for (int accepted_node : accepted_nodes) {
                result = (result + paths[accepted_node]) % MOD;
            }
        }
        return result;
    }
};

int main() {
    fstream in, out;
    in.open("problem3.in", ios_base::in);
    out.open("problem3.out", ios_base::out);

    int n, m, k;
    in >> n >> m >> k;
    automat aut(n);
    for (int i = 0; i < k; i++) {
        int accepted_node;
        in >> accepted_node;
        aut.accept_node(accepted_node - 1);
    }
    for (int i = 0; i < m; i++) {
        int from, to;
        char value;
        in >> from >> to >> value;
        aut.add_edge(from - 1, to - 1);
    }
    out << aut.count() << "\n";
}