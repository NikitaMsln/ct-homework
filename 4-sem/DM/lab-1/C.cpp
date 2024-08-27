#include <bits/stdc++.h>

using namespace std;

vector<int> prodl(int count, const vector<int>& a, const vector<int>& b) {
    vector<int> res(count, 0);
    for (int i = 0; i < res.size(); i++) {
        for (int j = 0; j <= i; j++) {
            int f = (j < a.size())? a[j] : 0;
            int s = (i - j < b.size())? b[i - j] : 0;
            res[i] = res[i] + f * s;
        }
    }
    return res;
}

int main() {
    int k;
    cin >> k;
    vector<int> a(k);
    vector<int> c(k);
    for (int i = 0; i < k; i++) {
        cin >> a[i];
    }
    for (int i = 0; i < k; i++) {
        cin >> c[i];
    }
    vector<int> q(k + 1);
    q[0] = 1;
    for (int i = 1; i <= k; i++) {
        q[i] = -c[i - 1];
    }
    auto p = prodl(k, a, q);
    while (p.size() > 1 && p.back() == 0) {
        p.pop_back();
    }
    cout << p.size() - 1 << "\n";
    for (auto x : p) {
        cout << x << " ";
    }
    cout << "\n";
    cout << q.size() - 1 << "\n";
    for (auto x : q) {
        cout << x << " ";
    }
    cout << "\n";
}