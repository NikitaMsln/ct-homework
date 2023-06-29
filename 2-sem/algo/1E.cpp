#include <bits/stdc++.h>

using namespace std;

int main() {
    fstream in, out;
    in.open("rmq.in", ios_base::in);
    out.open("rmq.out", ios_base::out);

    size_t n, m;
    in >> n >> m;

    vector<pair<size_t, size_t>> segment_beg(m), segment_end(m);
    vector<int32_t> infinums(m);
    vector<bool> used(m, false);
    map<int32_t, set<size_t>> segments;

    for (size_t i = 0; i < m; i++) {
        int32_t b, e, v;
        in >> b >> e >> v;
        segment_beg[i] = pair(i, b);
        segment_end[i] = pair(i, e);
        infinums[i] = v;
        segments[v] = set<size_t>();
    }

    sort(segment_beg.begin(), segment_beg.end(),
         [](const pair<size_t, size_t> &x, const pair<size_t, size_t> &y) { return x.second < y.second; });

    sort(segment_end.begin(), segment_end.end(),
         [](const pair<size_t, size_t> &x, const pair<size_t, size_t> &y) { return x.second < y.second; });

    multiset<int32_t> now_infinums;

    vector<int32_t> result(n);

    size_t beg_index = 0, end_index = 0, s = 0;
    for (size_t i = 1; i <= n; i++) {
        while (beg_index < m && segment_beg[beg_index].second == i) {
            segments[infinums[segment_beg[beg_index].first]].insert(segment_beg[beg_index].first);
            now_infinums.insert(infinums[segment_beg[beg_index].first]);
            beg_index++;
        }

        if (now_infinums.empty()) {
            result[i - 1] = 0;
        } else {
            int32_t value = *now_infinums.rbegin();
            if (!segments[value].empty()) {
                for (unsigned long it : segments[value]) {
                    used[it] = true;
                }
                segments[value].clear();
            }
            result[i - 1] = value;
        }

        while (end_index < m && segment_end[end_index].second == i) {
            if (!used[segment_end[end_index].first]) {
                out << "inconsistent\n";
                return 0;
            }
            segments[infinums[segment_end[end_index].first]].erase(segment_end[end_index].first);
            now_infinums.erase(now_infinums.find(infinums[segment_end[end_index].first]));
            end_index++;
        }
    }
    out << "consistent\n";
    for (size_t i = 0; i < n; i++) {
        out << result[i] << " ";
    }
    out << "\n";
}