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

    long long count(int length) {
        vector<bool> now_checked(nodes.size(), false);
        vector<long long> paths(nodes.size(), 0);
        for (int i : accepted_nodes) {
            now_checked[i] = true;
            paths[i] = 1;
        }
        for (int i = 0; i < length; i++) {
            vector<bool> next_checked(nodes.size(), false);
            vector<long long> next_paths(nodes.size(), 0);
            for (int j = 0; j < nodes.size(); j++) {
                if (now_checked[j]) {
                    for (size_t k = 0; k < nodes[j].reversed_edges.size(); k++) {
                        next_checked[nodes[j].reversed_edges[k]] = true;
                        next_paths[nodes[j].reversed_edges[k]] = (next_paths[nodes[j].reversed_edges[k]] + paths[j] * nodes[j].count[k]) % MOD;
                    }
                }
            }
            now_checked = next_checked;
            paths = next_paths;
        }
        return paths[0];
    }
};

int main() {
    fstream in, out;
    in.open("problem4.in", ios_base::in);
    out.open("problem4.out", ios_base::out);

    int n, m, k, l;
    in >> n >> m >> k >> l;
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
    out << aut.count(l) << "\n";
}