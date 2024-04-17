#include <bits/stdc++.h>

using namespace std;

inline constexpr uint64_t P = 257;
inline constexpr uint64_t MOD = 20995031;

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

inline constexpr uint64_t DIVP = fast_pow(P, MOD - 2);

vector<uint64_t> poly_hash(const string& s) {
    vector<uint64_t> res(s.length() + 1);
    res[0] = 0;
    uint64_t p = 1;
    for (int i = 0; i < s.length(); i++) {
        res[i + 1] = (res[i] + (p * (uint64_t)s[i]) % MOD) % MOD;
        p = (p * P) % MOD;
    }
    return res;
}

uint64_t hashof(const vector<uint64_t>& hash, int l, int r) {
    uint64_t res = (MOD + hash[r] - hash[l]) % MOD;
    if (l != 0) {
        res = (res * fast_pow(DIVP, l)) % MOD;
    }
    return res;
}

bool eq(int l1, int r1, int l2, int r2, const vector<uint64_t>& hash) {
    return r1 - l1 == r2 - l2 && (hashof(hash, l1 - 1, r1) == hashof(hash, l2 - 1, r2));
}

int main() {
    string s;
    cin >> s;
    auto hsh = poly_hash(s);
    int m;
    cin >> m;
    while (m--) {
        int a, b, c, d;
        cin >> a >> b >> c >> d;
        if (eq(a, b, c, d, hsh)) {
            cout << "Yes\n";
        } else {
            cout << "No\n";
        }
    }
}