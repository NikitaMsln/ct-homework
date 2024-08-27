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

template <size_t N, size_t M>
struct factorial {
    using next = factorial<N - 1, M>;

    static constexpr uint64_t value = (N * next::value) % M;

    static constexpr uint64_t divprod = fast_pow(value, M - 2);

    static uint64_t get_val(int val) {
        if (val == 0) {
            return value;
        } else {
            return next::get_val(val - 1);
        }
    }

    static uint64_t get_divp(int val) {
        if (val == 0) {
            return divprod;
        } else {
            return next::get_divp(val - 1);
        }
    }
};

template <size_t M>
struct factorial<0, M> {
    static constexpr uint64_t value = 1;

    static constexpr uint64_t divprod = 1;

    static uint64_t get_val(int val) {
        return value;
    }

    static uint64_t get_divp(int val) {
        return divprod;
    }
};

using fact = factorial<101, MOD>;

uint64_t factor(int v) {
    return fact::get_val(101 - v);
}

uint64_t divfactor(int v) {
    return fact::get_divp(101 - v);
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

vector<uint64_t> prodl(int count, const vector<uint64_t>& a, const vector<uint64_t>& b) {
    vector<uint64_t> res(count, 0);
    for (int i = 0; i < res.size(); i++) {
        for (int j = 0; j <= i; j++) {
            uint64_t f = (j < a.size())? a[j] : 0;
            uint64_t s = (i - j < b.size())? b[i - j] : 0;
            res[i] = (res[i] + f * s) % MOD;
        }
    }
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

vector<uint64_t> sqrt(int count, const vector<uint64_t>& a) {
    vector<uint64_t> res(count, 0);
    vector<uint64_t> pp(1, 1);
    static uint64_t div4 = fast_pow(4, MOD - 2);
    uint64_t pk = 1;
    for (int i = 0; i < count; i++) {
        uint64_t df = divfactor(i);
        df = df * df % MOD;
        uint64_t k = factor(2 * i) * df % MOD;
        k = k * fast_pow((MOD + 1 - 2 * i) % MOD, MOD - 2) % MOD;
        k = pk * k % MOD;
        if (i % 2 == 1) {
            k = (MOD - k) % MOD;
        }

        vector<uint64_t> ppk = pp;
        for (auto& x : ppk) {
            x = x * k % MOD;
        }

        res = sum(res, ppk);

        pk = pk * div4 % MOD;
        pp = prodl(count, pp, a);
    }
    while (res.size() < count) {
        res.push_back(0);
    }
    return res;
}

vector<uint64_t> exp(int count, const vector<uint64_t>& a) {
    vector<uint64_t> res(1, 1);
    assert(a[0] == 0);
    for (int i = 1; i < a.size(); i++) {
        vector<uint64_t> newp(count, 0);
        uint64_t k = 1;
        for (int j = 0; j < count; j += i) {
            newp[j] = (k * divfactor(j / i));
            k = (k * a[i]) % MOD;
        }
        res = prodl(count, res, newp);
    }
    while (res.size() < count) {
        res.push_back(0);
    }
    return res;
}

vector<uint64_t> ln(int count, const vector<uint64_t>& a) {
    assert(a[0] == 0);
    vector<uint64_t> res(1, 0);
    vector<uint64_t> pp = a;
    for (int i = 1; i < count; i++) {
        for (auto& x : pp) {
            x = (x * fast_pow(i, MOD - 2)) % MOD;
            if (i % 2 == 0) {
                x = (MOD - x) % MOD;
            }
        }
        res = sum(res, pp);
        for (auto& x : pp) {
            x = (x * i) % MOD;
            if (i % 2 == 0) {
                x = (MOD - x) % MOD;
            }
        }
        pp = prodl(count, pp, a);
    }
    while (res.size() < count) {
        res.push_back(0);
    }
    return res;
}

int main() {
    int n, m;
    cin >> n >> m;
    vector<uint64_t> P(n + 1);
    for (int i = 0; i <= n; i++) {
        cin >> P[i];
    }
    auto a1 = sqrt(m, P);
    for (auto x : a1) {
        cout << x << " ";
    }
    cout << "\n";
    auto a2 = exp(m, P);
    for (auto x : a2) {
        cout << x << " ";
    }
    cout << "\n";
    auto a3 = ln(m, P);
    for (auto x : a3) {
        cout << x << " ";
    }
    cout << "\n";
}