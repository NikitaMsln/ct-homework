#include <bits/stdc++.h>

using namespace std;



int main() {
    uint32_t n;
    cin >> n;
    vector<vector<bool>> edges(n, vector<bool>(n, false));
    list<uint32_t> q;
    q.push_back(0);
    edges[0][0] = true;
    for (uint32_t i = 1; i < n; i++) {
        edges[i][i] = true;
        q.push_back(i);
        string s;
        cin >> s;
        for (uint32_t j = 0; j < i; j++) {
            edges[i][j] = edges[j][i] = s[j] == '1';
        }
    }
    for (uint32_t i = 0; i < n; i++) {
        uint32_t curr = q.front();
        q.pop_front();
        if (!edges[curr][q.front()]) {
            auto first = q.begin();
            auto second = std::next(q.begin());
            while (!edges[curr][*first] || !edges[q.front()][*second]) {
                first++;
                second++;
            }
            for (auto l = q.begin(), r = first; l != r && l != second; l++, r--, second--) {
                std::swap(*l, *r);
            }
        }
        q.push_back(curr);
    }
    for (auto node : q) {
        cout << node + 1 << " ";
    }
    cout << "\n";
}