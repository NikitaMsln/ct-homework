#include <bits/stdc++.h>

using namespace std;

vector<size_t> prefix_func(const string& s) {
    size_t n = s.length();
    vector<size_t> res(n);
    res[0] = 0;
    for (int i = 1; i < n; i++) {
        int k = res[i - 1];
        while (k > 0 && s[i] != s[k])
            k = res[k - 1];
        if (s[i] == s[k])
            k++;
        res[i] = k;
    }
    return res;
}

int main() {
    string s;
    cin >> s;
    auto pf = prefix_func(s);
    for (int i = 0; i < pf.size(); i++) {
        cout << pf[i] << " ";
    }
    cout << "\n";
}
