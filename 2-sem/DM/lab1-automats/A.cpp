#include <bits/stdc++.h>

using namespace std;

int main() {
    fstream in, out;
    in.open("problem1.in", ios_base::in);
    out.open("problem1.out", ios_base::out);

    string word;
    in >> word;

    int n, m, k;
    in >> n >> m >> k;
    vector<vector<int>> edges(n, vector<int>(30, -1));
    vector<int> accepted_nodes(k);
    for (int i = 0; i < k; i++) {
        in >> accepted_nodes[i];
        accepted_nodes[i]--;
    }
    for (int i = 0; i < m; i++) {
        int from, to;
        char value;
        in >> from >> to >> value;
        edges[from - 1][value - 'a'] = to - 1;
    }
    int now_node = 0;
    for (char c : word) {
        now_node = edges[now_node][c - 'a'];
        if (now_node == -1) {
            out << "Rejects";
            return 0;
        }
    }
    for (size_t i = 0; i < k; i++) {
        if (now_node == accepted_nodes[i]) {
            out << "Accepts";
            return 0;
        }
    }
    out << "Rejects";
}