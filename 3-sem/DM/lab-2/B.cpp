#include <bits/stdc++.h>

using namespace std;

class snm {
public:
    snm(int n) : data_(n), count(n) {
        for (int i = 0; i < n; i++) {
            data_[i] = i;
        }
    }

    void merge(int a, int b) noexcept {
        int a_p = get_peak(a);
        int b_p = get_peak(b);
        if (a_p != b_p) {
            count --;
        }
        data_[a_p] = data_[b_p];
    }

    bool is_same(int a, int b) const noexcept {
        return get_peak(a) == get_peak(b);
    }

    int get_count() const noexcept {
        return count;
    }
private:
    mutable vector<int> data_;
    int count;

    int get_peak(int k) const noexcept {
        int l = k;
        while (data_[k] != k) {
            k = data_[k];
        }
        data_[l] = k;
        return k;
    }
};

struct edge {
    int f;
    int t;
    int i;
    long long s;
};

int main() {
    ifstream in("destroy.in");
    ofstream out("destroy.out");
    int n, m;
    long long s;
    in >> n >> m >> s;
    vector<edge> edges(m);
    snm components(n);
    for (int i = 0; i < m; i++) {
        in >> edges[i].f >> edges[i].t >> edges[i].s;
        edges[i].f--;
        edges[i].t--;
        edges[i].i = i;
    }
    sort(edges.begin(), edges.end(), [](const edge& a, const edge& b) {
        return a.s < b.s;
    });
    vector<bool> ost(m, false);
    for (int i = m - 1; components.get_count() > 1; i--) {
        if (!components.is_same(edges[i].f, edges[i].t)) {
            components.merge(edges[i].f, edges[i].t);
            ost[edges[i].i] = true;
        }
    }
    long long res_s = 0;
    vector<int> result;
    result.reserve(m);
    for (int i = 0; i < m && res_s + edges[i].s <= s; i++) {
        if (!ost[edges[i].i]) {
            result.push_back(edges[i].i + 1);
            res_s += edges[i].s;
        }
    }
    sort(result.begin(), result.end());
    out << result.size() << "\n";
    for (auto i : result) {
        out << i << " ";
    }
    out << "\n";
}