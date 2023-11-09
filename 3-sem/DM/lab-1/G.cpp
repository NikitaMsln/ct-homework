#include <bits/stdc++.h>

using namespace std;

void dfs(const vector<unordered_set<uint64_t>>& nodes, uint64_t curr, vector<uint64_t>& colors, uint64_t& max_color) {
    set<uint64_t> neighbour_colors;
    for (auto next : nodes[curr]) {
        if (colors[next] != 0) {
            neighbour_colors.insert(colors[next]);
        }
    }
    uint64_t min_c = 1;
    for (auto color : neighbour_colors) {
        if (min_c == color) {
            min_c++;
        } else if (min_c < color) {
            break;
        }
    }
    if (min_c > max_color) {
        max_color++;
    }
    colors[curr] = min_c;
    for (auto next : nodes[curr]) {
        if (colors[next] == 0) {
            dfs(nodes, next, colors, max_color);
        }
    }
}

vector<uint64_t> solve(uint64_t n, vector<unordered_set<uint64_t>> nodes) {
    uint64_t color = 1;
    vector<uint64_t> colors(n, 0);
    dfs(nodes, 0, colors, color);
    uint64_t mv = 0;
    for (auto& node : nodes) {
        if (node.size() > mv) {
            mv = node.size();
        }
    }
    mv |= 1;
    colors.insert(colors.begin(), mv);
    return colors;
}

void print(const vector<uint64_t> &v) {
    for (auto u: v) {
        cout << u << "\n";
    }
}

class test {
public:
    test(uint64_t count, uint64_t m) : n(count), m(m) {
        bool cont = true;
        while (cont) {
            nodes = vector<unordered_set<uint64_t>>(n);
            for (uint64_t i = 0; i < m; i++) {
                bool c = true;
                while (c) {
                    uint64_t u = rand() % n;
                    uint64_t v = rand() % n;
                    if (u != v && nodes[u].find(v) == nodes[u].end()) {
                        nodes[u].insert(v);
                        nodes[v].insert(u);
                        c = false;
                    }
                }
            }
            cont = !correct(nodes);
        }
    }

    void print() {
        cout << n << "\n";
        for (uint64_t i = 0; i < n; i++) {
            for (auto next : nodes[i]) {
                cout << i << " " << next << "\n";
            }
        }
    }

    bool hold() {
        cout << "start\n";
        clock_t start = clock();
        vector<uint64_t> res = solve(n, nodes);
        clock_t end = clock();
        cout << "end\n";
        vector<string> errors;
        uint64_t mv = 0;
        for (uint64_t i = 0; i < n; i++) {
            if (nodes[i].size() > res[0]) {
                errors.push_back("k (" + to_string(res[0]) + ") < d[" + to_string(i) + "]");
            }
            if (res[i + 1] > mv) {
                mv = res[i + 1];
            }
            for (auto next : nodes[i]) {
                if (res[i + 1] == res[next + 1]) {
                    errors.push_back("same colors of nodes: " + to_string(i) + " " + to_string(next));
                }
            }
        }
        long double time = (long double)(end - start) / (CLOCKS_PER_SEC);
        cout << "Time (n = " << n << ", m = " << m << "): " << time
                 << "\n";
        if (!errors.empty() || mv > res[0]) {
            cout << "Graph:\n";
            print();
            cout << "Result:\n";
            ::print(res);
            cout << "Errors:\n";
            if (mv > res[0]) {
                cout << "color count (" << mv << ") more then k (" << res[0] << ")\n";
            }
            for (const auto& s : errors) {
                cout << s << "\n";
            }
            return false;
        }
        return true;
    }
private:
    uint64_t n;
    uint64_t m;
    vector<unordered_set<uint64_t>> nodes;

    bool correct(vector<unordered_set<uint64_t>>& nodes_set) {
        vector<bool> visited(n, false);
        dfs(nodes_set, visited, 0);
        for (auto c : visited) {
            if (!c) {
                return false;
            }
        }
        return true;
    }

    void dfs(vector<unordered_set<uint64_t>>& nodes_set, vector<bool>& visited, uint64_t curr) {
        visited[curr] = true;
        for (auto next : nodes_set[curr]) {
            if (!visited[next]) {
                dfs(nodes_set, visited, next);
            }
        }
    }
};

void testing() {
    std::srand(std::time(nullptr));
    uint64_t cnt = 0;
    bool cont = true;
    while (cont) {
        if (++cnt % 1 == 0) {
            cout << "Test #" << cnt << "\n";
        }
        uint64_t n = rand() % 10000 + 10;
//        uint32_t n = 4;
        n |= 1;
        uint64_t m_max = min<uint64_t>(100'000, n * (n - 1) / 2);
        uint64_t m_min = n - 1;
        uint64_t m = rand() % (m_max - m_min) + m_min;
        cont = test(n, m).hold();
    }
}

int main() {
    uint64_t n, m;
    cin >> n >> m;
    vector<unordered_set<uint64_t>> nodes(n);
    for (uint64_t i = 0; i < m; i++) {
        uint64_t u, v;
        cin >> u >> v;
        nodes[u - 1].insert(v - 1);
        nodes[v - 1].insert(u - 1);
    }
    print(solve(n, nodes));
//    testing();
}