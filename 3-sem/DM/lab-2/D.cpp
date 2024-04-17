#include <bits/stdc++.h>

using namespace std;

bool check_2(const vector<bool>& sets, int n) {
    for (int i = sets.size(); i > 0; i--) {
        if (sets[i - 1]) {
            for (int j = 0; j < n; j++) {
                if ((i - 1) & (1 << j)) {
                    if (!sets[(i - 1) & (~(1 << j))]) {
                        return false;
                    }
                }
            }
        }
    }
    return true;
}

bool check_3(const vector<bool>& sets, const vector<vector<unsigned>>& sorted_sets) {
    for (int i = 1; i < sorted_sets.size() - 1; i++) {
        for (auto small : sorted_sets[i]) {
            for (auto large : sorted_sets[i + 1]) {
                bool finded = false;
                for (int c = 0; c < sorted_sets.size() && !finded; c++) {
                    if ((large & (1 << c)) && !(small & (1 << c))) {
                        finded = sets[small | (1 << c)];
                    }
                }
                if (!finded) {
                    return false;
                }
            }
        }
    }
    return true;
}

int main() {
    ifstream in("check.in");
    ofstream out("check.out");
    int n, m;
    in >> n >> m;
    vector<bool> sets((1 << n), false);
    vector<vector<unsigned>> sorted_sets(n + 1);
    for (int i = 0; i < m; i++) {
        int k;
        in >> k;
        unsigned c_set = 0;
        for (int j = 0; j < k; j++) {
            unsigned l;
            in >> l;
            c_set = c_set | (1 << (l - 1));
        }
        sorted_sets[k].push_back(c_set);
        sets[c_set] = true;
    }
    if (sets[0] && check_2(sets, n) && check_3(sets, sorted_sets)) {
        out << "YES\n";
    } else {
        out << "NO\n";
    }
}