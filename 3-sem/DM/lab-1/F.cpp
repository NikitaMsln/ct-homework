#include <bits/stdc++.h>

using namespace std;

vector<pair<uint32_t, uint32_t>> solve(uint32_t n, const vector<uint32_t>& code) {
    vector<uint32_t> code_c(n, 0);
    for (uint32_t i = 0; i < n - 2; i++) {
        code_c[code[i] - 1]++;
    }

    priority_queue<uint32_t, vector<uint32_t>, std::greater<>> fr;
    for (uint32_t i = n; i > 0; i--) {
        if (code_c[i - 1] == 0) {
            fr.push(i);
        }
    }

    vector<pair<uint32_t, uint32_t>> result;
    result.reserve(n - 1);

    for (uint32_t i = 0; i < n - 2; i++) {
        uint32_t last_fr = fr.top();
        fr.pop();
        result.push_back({last_fr, code[i]});
        if (--code_c[code[i] - 1] == 0) {
            fr.push(code[i]);
        }
    }
    uint32_t pl = fr.top();
    fr.pop();
    result.push_back({pl, fr.top()});
    return result;
}

void print(const vector<pair<uint32_t, uint32_t>>& v) {
    for (const auto& p : v) {
        cout << p.first << " " << p.second << "\n";
    }
}

void print(const vector<uint32_t>& vec, uint32_t add = 0) {
    for (uint32_t i = 0; i < vec.size(); i++) {
        cout << vec[i] + add << " ";
    }
    cout << "\n";
}

class tester {
public:
    tester(uint32_t count = 100) : n(count), nodes(n) {
        std::srand(std::time(nullptr));
        bool is_incorrect = true;
        while (is_incorrect) {
            for (uint32_t i = 0; i < n - 1; i++) {
                uint32_t j = i;
                while (j == i) {
                    j = rand() % (n - i - 1) + i + 1;
                }
                nodes[i].push_back(j);
                nodes[j].push_back(i);
            }
            is_incorrect = !correct();
        }
    }

    bool test() const {
        vector<pair<uint32_t, uint32_t>> res = solve(n, gen_code());
        vector<uint32_t> errors;
        for (uint32_t i = 0; i < res.size(); i++) {
            if (std::find(nodes[res[i].first - 1].begin(), nodes[res[i].first - 1].end(), res[i].second - 1) == nodes[res[i].first - 1].end()) {
                errors.push_back(i);
            }
        }
        if (!errors.empty()) {
            solve(n, gen_code());
            cout << "ERROR:\n";
            cout << "code:\n";
            print(gen_code());
            cout << "\nresult:\n";
            print(res);
            cout << "\nerrors indexes:\n";
            print(errors);
            return false;
        }
        return true;
    }

    vector<uint32_t> gen_code() const {
        struct node {
            uint32_t index;
            set<uint32_t> adj;
        };

        vector<node> nods(n);
        vector<node*> ordered_nodes(n);

        for (uint32_t i = 0; i < n; i++) {
            nods[i].index = i + 1;
            ordered_nodes[i] = &nods[i];
        }

        for (uint32_t i = 0; i < n; i++) {
            for (uint32_t j = 0; j < nodes[i].size(); j++) {
                nods[i].adj.insert(nodes[i][j] + 1);
            }
        }

        std::sort(ordered_nodes.begin(), ordered_nodes.end(), [](const node* a, const node* b) {
            return a->adj.size() < b->adj.size() || (a->adj.size() == b->adj.size() && a->index < b->index);
        });

        priority_queue<uint32_t, vector<uint32_t>, std::greater<>> res;
        for (auto ptr : ordered_nodes) {
            if (ptr->adj.size() > 1) {
                break;
            }
            res.push(ptr->index);
        }

        vector<uint32_t> result;
        result.reserve(n - 2);
        for (uint32_t i = 0; i < n - 2; i++) {
            uint32_t now_n = res.top();
            res.pop();
            node* adj = &nods[*nods[now_n - 1].adj.begin() - 1];
            result.push_back(adj->index);
            adj->adj.erase(now_n);
            if (adj->adj.size() == 1) {
                res.push(adj->index);
            }
        }
        return result;
    }

    bool correct() const {
        vector<bool> visited(n, false);
        if (!dfs(visited, 0, UINT32_MAX)) {
            return false;
        }
        for (bool a : visited) {
            if (!a) {
                return false;
            }
        }
        return true;
    }
private:
    uint32_t n;
    vector<vector<uint32_t>> nodes;

    bool dfs(vector<bool>& visited, uint32_t curr, uint32_t from) const {
        if (visited[curr]) {
            return false;
        }
        visited[curr] = true;
        bool res = true;
        for (uint32_t i = 0; i < nodes[curr].size() && res; i++) {
            if (nodes[curr][i] != from) {
                res = dfs(visited, nodes[curr][i], curr);
            }
        }
        return res;
    }
};

void testing() {
    bool cont = true;
    uint32_t cnt = 1;
    while (cont) {
        if (cnt++ % 100 == 0) {
            cout << "TEST " << cnt - 1 << "\n";
        }
        cont = tester(100000).test();
    }
}

int main() {
    uint32_t n;
    cin >> n;
    vector<uint32_t> code(n - 2);
    for (uint32_t i = 0; i < n - 2; i++) {
        cin >> code[i];
    }
    print(solve(n, code));
//    testing();
}
