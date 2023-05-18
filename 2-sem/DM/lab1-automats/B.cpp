#include <bits/stdc++.h>

using namespace std;

int main() {
    fstream in, out;
    in.open("problem2.in", ios_base::in);
    out.open("problem2.out", ios_base::out);

    string word;
    in >> word;

    int n, m, k;
    in >> n >> m >> k;
    vector<vector<set<int>>> edges(n, vector<set<int>>(30, set<int>()));
    vector<bool> accepted_nodes(n, false);
    for (int i = 0; i < k; i++) {
        int accepted_node;
        in >> accepted_node;
        accepted_nodes[accepted_node - 1] = true;
    }
    for (int i = 0; i < m; i++) {
        int from, to;
        char value;
        in >> from >> to >> value;
        edges[from - 1][value - 'a'].insert(to - 1);
    }

    set<int> now_nodes = {0};
    for (char c : word) {
        set<int> next_nodes;
        for (auto node : now_nodes) {
            next_nodes.merge(set(edges[node][c - 'a']));
        }
        now_nodes = next_nodes;
    }

    for (auto node : now_nodes) {
        if (accepted_nodes[node]) {
            out << "Accepts";
            return 0;
        }
    }
    out << "Rejects";
}