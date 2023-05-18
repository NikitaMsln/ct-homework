#include <bits/stdc++.h>

#define MOD ((long long)(1e9 + 7))

using namespace std;

bool isomorf(vector<vector<int>>& edges1, vector<vector<int>>& edges2, set<int>& accepted_nodes1, set<int>& accepted_nodes2) {
    if (edges1.size() != edges2.size() || accepted_nodes1.size() != accepted_nodes2.size()) {
        return false;
    }
    size_t n = edges2.size();
    vector<int> according_nodes(n, -1);
    according_nodes[0] = 0;
    queue<int> checking_nodes;
    checking_nodes.push(0);
    while (!checking_nodes.empty()) {
        int node1 = checking_nodes.front(), node2 = according_nodes[node1];
        checking_nodes.pop();
        for (size_t i = 0; i < edges1[node1].size(); i++) {
            if ((edges1[node1][i] == -1 && edges2[node2][i] != -1) || (edges1[node1][i] != -1 && edges2[node2][i] == -1)) {
                return false;
            } else if (edges1[node1][i] == -1) {
                continue;
            } else if (according_nodes[edges1[node1][i]] == -1) {
                according_nodes[edges1[node1][i]] = edges2[node2][i];
                checking_nodes.push(edges1[node1][i]);
            } else if (according_nodes[edges1[node1][i]] != edges2[node2][i]) {
                return false;
            }
        }
    }
    set<int> converted_accepted_nodes;
    for (auto node : accepted_nodes1) {
        converted_accepted_nodes.insert(according_nodes[node]);
    }
    return accepted_nodes2 == converted_accepted_nodes;
}



int main() {
    fstream in, out;
    in.open("isomorphism.in", ios_base::in);
    out.open("isomorphism.out", ios_base::out);

    int n1, m1, k1;
    in >> n1 >> m1 >> k1;
    vector<vector<int>> edges1(n1, vector<int>(30, -1));
    set<int> accepted_nodes1;
    for (int i = 0; i < k1; i++) {
        int accepted_node;
        in >> accepted_node;
        accepted_nodes1.insert(accepted_node - 1);
    }
    for (int i = 0; i < m1; i++) {
        int from, to;
        char c;
        in >> from >> to >> c;
        edges1[from - 1][c - 'a'] = to - 1;
    }

    int n2, m2, k2;
    in >> n2 >> m2 >> k2;
    vector<vector<int>> edges2(n2, vector<int>(30, -1));
    set<int> accepted_nodes2;
    for (int i = 0; i < k2; i++) {
        int accepted_node;
        in >> accepted_node;
        accepted_nodes2.insert(accepted_node - 1);
    }
    for (int i = 0; i < m2; i++) {
        int from, to;
        char c;
        in >> from >> to >> c;
        edges2[from - 1][c - 'a'] = to - 1;
    }

    if (isomorf(edges1, edges2, accepted_nodes1, accepted_nodes2)) {
        out << "YES\n";
    } else {
        out << "NO\n";
    }
}