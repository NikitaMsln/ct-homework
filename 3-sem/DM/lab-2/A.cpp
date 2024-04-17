#include <bits/stdc++.h>

using namespace std;

int main() {
    ifstream in("schedule.in");
    ofstream out("schedule.out");
    long long n;
    in >> n;
    vector<pair<long long, long long>> d(n);
    set<int, std::greater<>> t;
    for (int i = 0; i < n; i++) {
        in >> d[i].first >> d[i].second;
        t.insert(i + 1);
    }
    sort(d.begin(), d.end(), [](const pair<long long, long long>& a, const pair<long long, long long>& b) {
        return a.second > b.second || (a.second == b.second && a.first > b.second);
    });
    long long result = 0;
    for (int i = 0; i < n; i++) {
        auto it = t.lower_bound(d[i].first);
        if (it != t.end() && *it <= d[i].first) {
            t.erase(it);
        } else {
            result += d[i].second;
        }
    }
    out << result << "\n";
}
