#include <bits/stdc++.h>

using namespace std;

inline constexpr uint64_t MOD = 998'244'353;

constexpr uint64_t fast_pow(uint64_t a, uint64_t b) {
    if (b == 0) {
        return 1;
    }
    uint64_t h = fast_pow(a, b / 2);
    if (b % 2 == 0) {
        return (h * h) % MOD;
    } else {
        return (((h * h) % MOD) * a) % MOD;
    }
}

void pop_zero(vector<uint64_t>& vec) {
    while (vec.size() > 1 && vec.back() == 0) {
        vec.pop_back();
    }
}

vector<uint64_t> sum(const vector<uint64_t>& a, const vector<uint64_t>& b) {
    vector<uint64_t> res;
    int i = 0;
    for (; i < a.size() && i < b.size(); i++) res.push_back((a[i] + b[i]) % MOD);
    for (; i < a.size(); i++) res.push_back(a[i]);
    for (; i < b.size(); i++) res.push_back(b[i]);
    pop_zero(res);
    return res;
}

vector<uint64_t> prod(const vector<uint64_t>& a, const vector<uint64_t>& b) {
    vector<uint64_t> res(a.size() + b.size() + 1, 0);
    for (int i = 0; i < res.size(); i++) {
        for (int j = 0; j <= i; j++) {
            uint64_t f = (j < a.size())? a[j] : 0;
            uint64_t s = (i - j < b.size())? b[i - j] : 0;
            res[i] = (res[i] + f * s) % MOD;
        }
    }
    pop_zero(res);
    return res;
}

vector<uint64_t> div(int count, const vector<uint64_t>& a, const vector<uint64_t>& b) {
    vector<uint64_t> res(count);
    uint64_t divm = fast_pow(b[0], MOD - 2);
    res[0] = a[0] * divm % MOD;
    for (int i = 1; i < count; i++) {
        res[i] = (i < a.size())? a[i] : 0;
        for (int j = 0; j < i; j++) {
            uint64_t p = (i - j < b.size())? b[i - j] : 0;
            res[i] = (MOD + res[i] - res[j] * p % MOD) % MOD;
        }
        res[i] = res[i] * divm % MOD;
    }
    return res;
}

int main() {
    uint64_t a_size, b_size;
    cin >> a_size >> b_size;
    vector<uint64_t> a(a_size + 1), b(b_size + 1);
    for (auto& e : a) {
        cin >> e;
    }
    for (auto& e : b) {
        cin >> e;
    }
    auto sm = sum(a, b);
    cout << sm.size() - 1 << "\n";
    for (auto e : sm) {
        cout << e << " ";
    }
    cout << "\n";
    auto pd = prod(a, b);
    cout << pd.size() - 1 << "\n";
    for (auto e : pd) {
        cout << e << " ";
    }
    cout << "\n";
    auto dv = div(1000, a, b);
    for (auto e : dv) {
        cout << e << " ";
    }
    cout << "\n";
}