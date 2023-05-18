#include <bits/stdc++.h>

using namespace std;

int main() {
    fstream in, out;
    in.open("minimization.in", ios_base::in);
    out.open("minimization.out", ios_base::out);

    int n, m, k;
    in >> n >> m >> k;
    vector<vector<int>> inv_edges(n, vector<int>(30, -1));
    vector<vector<set<int>>> edges(n, vector<set<int>>(30));
    vector<bool> is_accepted(n, false);
    for (int i = 0; i < k; i++) {
        int accepted_node;
        in >> accepted_node;
        is_accepted[accepted_node - 1] = true;
    }
    for (int i = 0; i < m; i++) {
        int from, to;
        char c;
        in >> from >> to >> c;
        inv_edges[from - 1][c - 'a'] = to - 1;
        edges[to - 1][c - 'a'].insert(from - 1);
    }

    vector<set<int>> P(2);
    vector<int> Class(n);
    for (int i = 0; i < n; i++) {
        if (is_accepted[i]) {
            P[1].insert(i);
            Class[i] = 1;
        } else {
            P[0].insert(i);
            Class[i] = 0;
        }
    }
    if (P[0].size() == 0) {
        swap(P[0], P[1]);
        P.pop_back();
        for (int i = 0; i < Class.size(); i++) {
            Class[i] = 0;
        }
    } else if (P[1].size() == 0) {
        P.pop_back();
    }
    queue<int> class_queue, char_queue;
    for (int i = 0; i < 30; i++) {
        class_queue.push(0);
        char_queue.push(i);
        if (P.size() > 1) {
            class_queue.push(1);
            char_queue.push(i);
        }
    }
    while (!class_queue.empty()) {
        int C = class_queue.front(), a = char_queue.front();
        class_queue.pop();
        char_queue.pop();
        vector<vector<int>> Involved(P.size());
        for (auto q : P[C]) {
            for (auto r : edges[q][a]) {
                if (r == -1) continue;
                int i = Class[r];
                Involved[i].push_back(r);
            }
        }
        for (size_t i = 0; i < Involved.size(); i++) {
            if (Involved[i].size() < P[i].size() && Involved[i].size() > 0) {
                P.push_back({});
                int j = P.size() - 1;
                for (auto r : Involved[i]) {
                    P[i].erase(r);
                    P[j].insert(r);
                }
                if (P[j].size() > P[i].size()) {
                    swap(P[i], P[j]);
                }
                for (auto r : P[j]) {
                    Class[r] = j;
                }
                for (int i = 0; i < 30; i++) {
                    class_queue.push(j);
                    char_queue.push(i);
                }
            }
        }
    }
    vector<vector<int>> new_edges(P.size(), vector<int>(30, -1));
    int edges_c = 0;
    for (int i = 0; i < inv_edges.size(); i++) {
        for (int a = 0; a < 30; a++) {
            if (inv_edges[i][a] != -1 && new_edges[Class[i]][a] == -1) {
                new_edges[Class[i]][a] = Class[inv_edges[i][a]];
                edges_c++;
            }
        }
    }
    vector<int> accepted;
    for (int i = 0; i < P.size(); i++) {
        if (is_accepted[(*P[i].begin())]) {
            accepted.push_back(i);
        }
    }
    out << P.size() << " " << edges_c << " " << accepted.size() << "\n";
    for (int i = 0; i < accepted.size(); i++) {
        out << accepted[i] + 1 << " ";
    }
    out << "\n";

    for (int i = 0; i < new_edges.size(); i++) {
        for (int a = 0; a < 30; a++) {
            if (new_edges[i][a] != -1) {
                out << i + 1 << " " << new_edges[i][a] + 1 << " " << (char)(a + 'a') << "\n";
            }
        }
    }
}