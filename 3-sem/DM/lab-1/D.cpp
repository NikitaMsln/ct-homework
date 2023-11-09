#include <bits/stdc++.h>

using namespace std;

deque<uint32_t>::iterator next_in_cycle(deque<uint32_t>::iterator a, deque<uint32_t>& deq) {
    a++;
    if (a == deq.end()) {
        return deq.begin();
    }
    return a;
}

vector<uint32_t> solve(uint32_t n, const vector<vector<bool>>& edges) {
    deque<uint32_t> res;
    res.push_back(0);
    for (uint32_t i = 1; i < n; i++) {
        auto place = res.begin();
        while (place != res.end() && edges[*place][i]) {
            place++;
        }
        res.insert(place, i);
    }
    deque<uint32_t> other;
    while (!edges[res.back()][res.front()]) {
        other.push_front(res.back());
        res.pop_back();
    }
    auto lit = res.begin();
    auto it = next_in_cycle(lit, res);
    while (!other.empty()) {
        auto ret_it = other.begin();
        for (; ret_it != other.end(); ret_it++) {
            if (edges[*ret_it][*it]) {
                break;
            }
        }
        if (ret_it != other.end()) {
            uint32_t last = *ret_it;
            it = lit;
            while (*it != last) {
                it++;
                it = res.insert(it, other.front());
                other.pop_front();
            }
        }
        lit = it;
        it = next_in_cycle(lit, res);
    }
    return std::vector<uint32_t>(res.begin(), res.end());
}

void print(const vector<uint32_t>& vec, uint32_t add = 0) {
    for (uint32_t i = 0; i < vec.size(); i++) {
        cout << vec[i] + add << " ";
    }
    cout << "\n";
}



class tester {
public:
    tester(uint32_t count = 100) : n(count), edges(vector<vector<bool>>(count, vector<bool>(count, false))) {
        std::srand(std::time(nullptr));
        bool is_incorrect = true;
        while (is_incorrect) {
            std::fill(edges.begin(), edges.end(), vector<bool>(n, false));
            for (uint32_t i = 0; i < n; i++) {
                for (uint32_t j = i + 1; j < n; j++) {
                    edges[i][j] = !(edges[j][i] = (rand() % 2 == 0));
                }
            }
            is_incorrect = !single_comp();
        }
    }

    void print_edges() {
        cout << "\t";
        for (uint32_t i = 0; i < n; i++) {
            cout << (i + 1) << "\t";
        }
        cout << "\n";
        for (uint32_t i = 0; i < n; i++) {
            cout << (i + 1) << ":\t";
            for (uint32_t j = 0; j < n; j++) {
                cout << edges[i][j] << "\t";
            }
            cout << "\n";
        }
    }

    bool test() {
        vector<uint32_t> res = solve(n, edges);
        vector<uint32_t> errors;
        for (uint32_t i = 1; i < res.size(); i++) {
            if (!edges[res[i - 1]][res[i]]) {
                errors.push_back(i);
            }
        }
        if (!edges[res.back()][res.front()]) {
            errors.push_back(0);
        }
        if (!errors.empty()) {
            solve(n, edges);
            cout << "ERROR:\n";
            cout << "edges:\n";
            print_edges();
            cout << "\nresult:\n";
            print(res, 1);
            cout << "\nerrors indexes:\n";
            print(errors);
            return false;
        }
        return true;
    }

    bool single_comp() {
        vector<bool> visited(edges.size(), false);
        vector<uint32_t> order;
        dfs1(visited, order, 0);
        for (uint32_t i = 1; i < edges.size(); i++) {
            if (!visited[i]) {
                return false;
            }
        }
        uint32_t comp = 1;
        vector<uint32_t> component(edges.size(), 0);
        dfs2(component, comp, order.front());
        for (uint32_t i = order.size(); i > 0; i--) {
            if (component[order[i - 1]] == 0) {
                return false;
            }
        }
        return true;
    }
private:
    uint32_t n = 100;
    vector<vector<bool>> edges = vector<vector<bool>>(n, vector<bool>(n, false));

    void dfs1(vector<bool>& visited, vector<uint32_t>& order, uint32_t curr) {
        visited[curr] = true;
        for (uint32_t i = 0; i < edges.size(); i++) {
            if (!visited[i] && edges[i][curr]) {
                dfs1(visited, order, i);
            }
        }
        order.push_back(curr);
    }

    void dfs2(vector<uint32_t>& component, uint32_t curr_comp, uint32_t curr) {
        component[curr] = curr_comp;
        for (uint32_t i = 0; i < edges.size(); i++) {
            if (component[i] == 0 && edges[curr][i]) {
                dfs2(component, curr_comp, i);
            }
        }
    }
};

void testing() {
    bool cont = true;
    uint32_t cnt = 1;
    while (cont) {
        if (cnt++ % 100 == 0) {
            cout << "TEST " << cnt - 1 << "\n";
        }
        cont = tester().test();
    }
}

int main() {
    uint32_t n;
    cin >> n;
    vector<vector<bool>> edges(n, vector<bool>(n, false));
    for (uint32_t i = 1; i < n; i++) {
        string s;
        cin >> s;
        for (uint32_t j = 0; j < i; j++) {
            if (s[j] == '1') {
                edges[i][j] = true;
            } else {
                edges[j][i] = true;
            }
        }
    }
    print(solve(n, edges), 1);
//    testing();
}
