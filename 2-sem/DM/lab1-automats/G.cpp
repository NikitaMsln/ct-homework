#include <bits/stdc++.h>

using namespace std;

bool equals(vector<vector<int>>& edges1, vector<vector<int>>& edges2, vector<bool>& is_accepted1, vector<bool>& is_accepted2) {
    queue<int> nodes1, nodes2;
    nodes1.push(0);
    nodes2.push(0);
    vector<vector<int>> used(edges1.size(), vector<int>(edges2.size(), 0));
    while(!nodes1.empty()) {
        int node1 = nodes1.front(), node2 = nodes2.front();
        nodes1.pop();
        nodes2.pop();
        if (is_accepted1[node1] != is_accepted2[node2]) {
            return false;
        }
        used[node1][node2]++;
        for (size_t i = 0; i < edges1[node1].size(); i++) {
            if (used[edges1[node1][i]][edges2[node2][i]] < 10) {
                nodes1.push(edges1[node1][i]);
                nodes2.push(edges2[node2][i]);
            }
        }
    }
    return true;
}



int main() {
    fstream in, out;
    in.open("equivalence.in", ios_base::in);
    out.open("equivalence.out", ios_base::out);

    int n1, m1, k1;
    in >> n1 >> m1 >> k1;
    vector<vector<int>> edges1(n1 + 1, vector<int>(30, n1));
    vector<bool> is_accepted1(n1 + 1, false);
    for (int i = 0; i < k1; i++) {
        int accepted_node;
        in >> accepted_node;
        is_accepted1[accepted_node - 1] = true;
    }
    for (int i = 0; i < m1; i++) {
        int from, to;
        char c;
        in >> from >> to >> c;
        edges1[from - 1][c - 'a'] = to - 1;
    }

    int n2, m2, k2;
    in >> n2 >> m2 >> k2;
    vector<vector<int>> edges2(n2 + 1, vector<int>(30, n2));
    vector<bool> is_accepted2(n2 + 1, false);
    for (int i = 0; i < k2; i++) {
        int accepted_node;
        in >> accepted_node;
        is_accepted2[accepted_node - 1] = true;
    }
    for (int i = 0; i < m2; i++) {
        int from, to;
        char c;
        in >> from >> to >> c;
        edges2[from - 1][c - 'a'] = to - 1;
    }

    if (equals(edges1, edges2, is_accepted1, is_accepted2)) {
        out << "YES\n";
    } else {
        out << "NO\n";
    }
}