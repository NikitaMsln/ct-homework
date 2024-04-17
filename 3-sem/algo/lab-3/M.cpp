#include <bits/stdc++.h>

using namespace std;

template <uint64_t P, uint64_t MOD>
struct polyhash {
    static constexpr uint64_t fast_pow(uint64_t a, uint64_t b) {
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

    static constexpr uint64_t DIVP = fast_pow(P, MOD - 2);

    static vector<uint64_t> poly_hash(const string& s) {
        vector<uint64_t> res(s.length() + 1);
        res[0] = 0;
        uint64_t p = 1;
        for (int i = 0; i < s.length(); i++) {
            res[i + 1] = (res[i] + (p * (uint64_t)s[i]) % MOD) % MOD;
            p = (p * P) % MOD;
        }
        return res;
    }

    static uint64_t hashof(const vector<uint64_t>& hash, int l, int r) {
        uint64_t res = (MOD + hash[r] - hash[l]) % MOD;
        if (l != 0) {
            res = (res * fast_pow(DIVP, l)) % MOD;
        }
        return res;
    }
};

using hash1 = polyhash<257, 20995031>;
using hash2 = polyhash<263, 1900000097>;

int has_common_substr(const vector<vector<uint64_t>>& h1, const vector<vector<uint64_t>>& h2, int length) {
    vector<pair<uint64_t, uint64_t>> s1;
    s1.reserve(max(0, (int)h1[0].size() - length));
    vector<pair<uint64_t, uint64_t>> s2;
    s2.reserve(max(0, (int)h1[1].size() - length));

    for (int j = 0; j + length < h1[0].size(); j++) {
        s1.push_back({hash1::hashof(h1[0], j, j + length), hash2::hashof(h2[0], j, j + length)});
    }

    for (int j = 0; j + length < h1[1].size(); j++) {
        s2.push_back({hash1::hashof(h1[1], j, j + length), hash2::hashof(h2[1], j, j + length)});
    }

    sort(s2.begin(), s2.end());

    for (int i = 0; i < s1.size(); i++) {
        auto it = lower_bound(s2.begin(), s2.end(), s1[i]);
        if (it != s2.end() && *it == s1[i]) {
            return i;
        }
    }
    return -1;
}

int main() {
    vector<string> s(2);
    vector<vector<uint64_t>> h1(2);
    vector<vector<uint64_t>> h2(2);
    for (int i = 0; i < 2; i++) {
        cin >> s[i];
        h1[i] = hash1::poly_hash(s[i]);
        h2[i] = hash2::poly_hash(s[i]);
    }
    int l = 0, r = s[0].size() + 1;
    int lv = 0;
    while (l < r - 1) {
        int m = (l + r) / 2;
        int k = has_common_substr(h1, h2, m);
        if (k != -1) {
            lv = k;
            l = m;
        } else {
            r = m;
        }
    }
    cout << s[0].substr(lv, l) << "\n";
}
