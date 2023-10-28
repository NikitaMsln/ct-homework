#include <bits/stdc++.h>

using namespace std;

struct edge {
    uint32_t p1;
    uint32_t p2;
    uint32_t weight;
};

class snm {
public:
    snm(uint32_t n) : comp(n) {
        for (uint32_t i = 0; i < n; i++) {
            comp[i] = i;
        }
    }

    uint32_t get(uint32_t i) {
        uint32_t n = i;
        while (comp[n] != n) {
            n = comp[n];
        }
        comp[i] = n;
        return n;
    }

    void merge(uint32_t i, uint32_t j) {
        uint32_t i1 = get(i);
        uint32_t i2 = get(j);
        comp[i1] = i2;
    }
private:
    vector<uint32_t> comp;
};

int main() {
    uint32_t n, m;
    cin >> n >> m;
    vector<edge> edges;
    edges.reserve(m);
    for (uint32_t i = 0; i < m; i++) {
        edge q{};
        cin >> q.p1 >> q.p2 >> q.weight;
        q.p1--;
        q.p2--;
        edges.push_back(q);
    }
    sort(edges.begin(), edges.end(), [](const edge &a, const edge &b) { return a.weight < b.weight; });
    snm comp(n);
    uint32_t k = 0;
    uint64_t res = 0;
    for (uint32_t i = n - 1; i > 0 && k < m; k++) {
        auto &curr = edges[k];
        if (comp.get(curr.p1) != comp.get(curr.p2)) {
            comp.merge(curr.p1, curr.p2);
            res += curr.weight;
            i--;
        }
    }
    cout << res << "\n";
}