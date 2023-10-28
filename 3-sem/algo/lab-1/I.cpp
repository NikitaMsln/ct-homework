#include <bits/stdc++.h>

using namespace std;

struct point {
    uint32_t x;
    uint32_t y;
};

long double dist(const point &a, const point &b) {
    return sqrt((a.x - b.x) * (a.x - b.x) + (a.y - b.y) * (a.y - b.y));
}

struct compare {
    bool operator()(const pair<uint32_t, long double> &a, const pair<uint32_t, long double> &b) {
        return a.second > b.second;
    }
};

int main() {
    uint32_t n;
    cin >> n;
    vector<point> points(n);
    for (uint32_t i = 0; i < n; i++) {
        cin >> points[i].x;
        cin >> points[i].y;
    }
    long double res = 0;
    priority_queue<pair<uint32_t, long double>, vector<pair<uint32_t, long double>>, compare> q;
    q.push({0, 0});
    for (uint32_t i = 1; i < n; i++) {
        q.push({i, std::numeric_limits<double>::max()});
    }
    while (!q.empty()) {
        auto curr = q.top();
        q.pop();
        res += curr.second;
        priority_queue<pair<uint32_t, long double>, vector<pair<uint32_t, long double>>, compare> new_q;
        while (!q.empty()) {
            auto next = q.top();
            q.pop();
            new_q.push({next.first, std::min(next.second, dist(points[curr.first], points[next.first]))});
        }
        q = std::move(new_q);
    }
    cout << res << "\n";
}