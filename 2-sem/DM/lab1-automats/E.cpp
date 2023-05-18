#include <bits/stdc++.h>

#define MOD ((long long)(1e9 + 7))

using namespace std;

int main() {
    fstream in, out;
    in.open("problem5.in", ios_base::in);
    out.open("problem5.out", ios_base::out);

    int n, m, k, l;
    in >> n >> m >> k >> l;
    vector<vector<set<int>>> edges(n, vector<set<int>>(30, set<int>()));
    vector<int> accepted_nodes(k);
    for (int i = 0; i < k; i++) {
        in >> accepted_nodes[i];
        accepted_nodes[i]--;
    }
    for (int i = 0; i < m; i++) {
        int from, to;
        char c;
        in >> from >> to >> c;
        edges[from - 1][c - 'a'].insert(to - 1);
    }

    vector<set<int>> now_nodes = {{0}};
    vector<long long> count = {1};
    while (l--) {
        vector<set<int>> next_nodes;
        vector<long long> next_counts;
        for (size_t i = 0; i < now_nodes.size(); i++) {
            for (size_t j = 0; j < 30; j++) {
                set<int> next_node;
                for (auto node : now_nodes[i]) {
                    next_node.insert(edges[node][j].begin(), edges[node][j].end());
                }
                bool already_inside = false;
                for (size_t p = 0; p < next_nodes.size(); p++) {
                    if (next_nodes[p] == next_node) {
                        next_counts[p] = (next_counts[p] + count[i]) % MOD;
                        already_inside = true;
                        break;
                    }
                }
                if (!already_inside){
                    next_nodes.push_back(next_node);
                    next_counts.push_back(count[i]);
                }
            }
        }
        now_nodes = next_nodes;
        count = next_counts;
    }
    long long result = 0;
    for (size_t i = 0; i < now_nodes.size(); i++) {
        for (auto accepted_node : accepted_nodes) {
            if (now_nodes[i].count(accepted_node) > 0) {
                result = (result + count[i]) % MOD;
                break;
            }
        }
    }
    out << result << "\n";
}