#include <bits/stdc++.h>

using namespace std;

typedef long long ld_t;

bool corr_rebalance(const vector<set<int>> &edges, vector<int> &l, int c, vector<bool> &visited) {
    if (visited[c]) {
        return false;
    }
    visited[c] = true;
    int cur = l[c];
    for (auto n: edges[cur]) {
        if (l[n] == -1 || corr_rebalance(edges, l, n, visited)) {
            l[n] = l[c];
            l[c] = -1;
            return true;
        }
    }
    return false;
}

vector<int> corr_solve(int n, const vector<pair<int, ld_t>>& w, const vector<set<int>>& edges, const vector<set<int>>& inv_edges) {
    vector<int> l(n, -1);

    for (int i = 0; i < n; i++) {
        for (auto next: edges[w[i].first]) {
            vector<bool> visited(n, false);
            if (l[next] == -1 || corr_rebalance(edges, l, next, visited)) {
                l[next] = w[i].first;
                break;
            }
        }
    }
    vector<int> result(n, -1);
    for (int i = 0; i < n; i++) {
        if (l[i] != -1) {
            result[l[i]] = i;
        }
    }
    return result;
}

bool rebalance(int curr, const vector<set<int>>& edges, vector<bool>& visited, vector<int>& l) {
    if (visited[curr]) {
        return false;
    }
    visited[curr] = true;
    for (auto next : edges[curr]) {
        if (l[next] == -1 || rebalance(l[next], edges, visited, l)) {
            l[next] = curr;
            return true;
        }
    }
    return false;
}

vector<int> solve(int n, const vector<pair<int, ld_t>>& w, const vector<set<int>>& edges, const vector<set<int>>& inv_edges) {
    vector<bool> visited;
    vector<int> l(n, -1);
    for (int i = 0; i < n; i++) {
        visited = vector<bool>(n, false);
        rebalance(w[i].first, edges, visited, l);
    }
    vector<int> result(n, -1);
    for (int i = 0; i < n; i++) {
        if (l[i] != -1) {
            result[l[i]] = i;
        }
    }
    return result;
}

class test {
public:
    test(int count = 10) : n(count), w(n), edges(n), inv_edges(n) {
        srand(time(nullptr));
        for (int i = 0; i < n; i++) {
            w[i].first = i;
            w[i].second = rand() % 1000;
        }
        sort(w.begin(), w.end(), [](const pair<int, ld_t> &a, const pair<int, ld_t> &b) {
            return a.second > b.second;
        });
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                if (rand() % 2) {
                    edges[i].insert(j);
                    inv_edges[j].insert(i);
                }
            }
        }
    }

    bool hold() {
        vector<string> errors;
        auto result = solve(n, w, edges, inv_edges);
        auto answer = corr_solve(n, w, edges, inv_edges);
        sort(w.begin(), w.end(), [](const pair<int, ld_t> &a, const pair<int, ld_t> &b) {
            return a.first < b.first;
        });
        long long answer_s = 0;
        long long result_s = 0;
        for (int i = 0; i < n; i++) {
            if (result[i] != -1) {
                result_s += w[i].second * w[i].second;
            }
            if (answer[i] != -1) {
                answer_s += w[i].second * w[i].second;
            }
        }
        if (result_s != answer_s) {
            errors.push_back(string("Incorrect weight data: answer -- " + to_string(result_s) + string(", correct -- ") +
                                            to_string(answer_s)));
        }
        vector<set<int>> l(n);
        for (int i = 0; i < n; i++) {
            if (result[i] != -1) {
                if (edges[i].find(result[i]) == edges[i].end()) {
                    errors.push_back("Unexisted edge: " + to_string(i) + " " + to_string(result[i]));
                } else {
                    l[result[i]].insert(i);
                    if (l[result[i]].size() > 1) {
                        string n;
                        for (auto p : l[result[i]]) {
                            n += " " + to_string(p);
                        }
                        errors.push_back("Too many edges to node " + to_string(result[i]) + ":" + n);
                    }
                }
            }
        }
        if (!errors.empty()) {
            cout << n << "\n";
            for (int i = 0; i < n; i++) {
                cout << w[i].second << " ";
            }
            cout << "\n";
            for (int i = 0; i < n; i++) {
                cout << edges[i].size();
                for (auto j : edges[i]) {
                    cout << " " << j + 1;
                }
                cout << "\n";
            }
            cout << "[ANSWER]";
            for (auto p : result) {
                cout << " " << p + 1;
            }
            cout << "\n";
            cout << "[CORRECT ANSWER]";
            for (auto p : answer) {
                cout << " " << p + 1;
            }
            cout << "\n";
            for (auto& s : errors) {
                cout << "[ERROR] " << s << "\n";
            }
            return false;
        }
        return true;
    }
private:
    int n;
    vector<pair<int, ld_t>> w;
    vector<set<int>> edges;
    vector<set<int>> inv_edges;
};

void testing() {
    int cnt = 0;
    while (true) {
        cnt++;
        if (cnt % 1 == 0) {
            cout << "[Test #" << cnt << "]\n";
        }
        if (!test(1000).hold()) {
            return;
        }
    }
}

int main() {
    ifstream in("matching.in");
    ofstream out("matching.out");
    int n;
    in >> n;
    vector<pair<int, ld_t>> w(n);
    for (int i = 0; i < n; i++) {
        in >> w[i].second;
        w[i].first = i;
    }
    vector<set<int>> edges(n);
    vector<set<int>> inv_edges(n);
    for (int i = 0; i < n; i++) {
        int k;
        in >> k;
        for (int j = 0; j < k; j++) {
            int a;
            in >> a;
            edges[i].insert(a - 1);
            inv_edges[a - 1].insert(i);
        }
    }
    sort(w.begin(), w.end(), [](const pair<int, ld_t> &a, const pair<int, ld_t> &b) {
        return a.second > b.second;
    });

    auto result = solve(n, w, edges, inv_edges);
    for (int i = 0; i < n; i++) {
        out << (result[i] + 1) << " ";
    }
    out << "\n";
//    testing();
}