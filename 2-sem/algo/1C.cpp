#include <bits/stdc++.h>

using namespace std;

unsigned int a, b;
unsigned int cur = 0;
unsigned int nextRand17() {
    cur = cur * a + b;
    return cur >> 15;
}

unsigned int nextRand24() {
    cur = cur * a + b;
    return cur >> 8;
}

vector<unsigned int> merge(const vector<unsigned int>& x, const vector<unsigned int>& y) {
    vector<unsigned int> result;
    for (int i = 0, j = 0; i < x.size() || j < y.size();) {
        if (i == x.size()) {
            result.push_back(y[j++]);
        } else if (j == y.size() || x[i] < y[j]) {
            result.push_back(x[i++]);
        } else {
            result.push_back(y[j++]);
        }
    }
    return result;
}

void find(vector<unsigned int>& res, const vector<pair<pair<unsigned int, unsigned int>, vector<unsigned int>>>& tree, unsigned int x, unsigned int y, unsigned int i) {
    if (i >= tree.size()) {
        return;
    }
    if (tree[i].first.first >= x && tree[i].first.second <= y) {
        res.push_back(i);
        return;
    } else if (tree[i].first.first > y || tree[i].first.second < x) {
        return;
    }
    find(res, tree, x, y, i * 2 + 1);
    find(res, tree, x, y, i * 2 + 2);
}

int ub(const vector<unsigned int>& arr, unsigned int v) {
    int l = -1, r = arr.size();
    while (l + 1 < r) {
        int s = (l + r) / 2;
        if (arr[s] <= v) {
            l = s;
        } else {
            r = s;
        }
    }
    return r;
}

int lb(const vector<unsigned int>& arr, unsigned int v) {
    int l = -1, r = arr.size();
    while (l + 1 < r) {
        int s = (l + r) / 2;
        if (arr[s] < v) {
            l = s;
        } else {
            r = s;
        }
    }
    return r;
}

int main() {
    int q;
    cin >> q;
    cin >> a >> b;
    vector<pair<pair<unsigned int, unsigned int>, vector<unsigned int>>> f(1 << 17);
    for (int i = 0; i < 1 << 17; i++) {
        unsigned int p = nextRand24();
        f[i].first = pair<unsigned int, unsigned int>(p, p);
        f[i].second = vector<unsigned int>(1, i);
    }

    sort(f.begin(), f.end(), [](const pair<pair<unsigned int, unsigned int>, vector<unsigned int>>& a, const pair<pair<unsigned int, unsigned int>, vector<unsigned int>>& b) {return a.first.first > b.first.first;});

    for (int i = 1, k = 0; i <= 17; i++) {
        while (f[k].second.size() < 1 << i) {
            pair<pair<unsigned int, unsigned int>, vector<unsigned int>> p;
            p.first = pair<unsigned int, unsigned int>(f[k+1].first.first, f[k].first.second);
            p.second = merge(f[k].second, f[k+1].second);
            f.push_back(p);
            k += 2;
        }
    }
    reverse(f.begin(), f.end());

    unsigned int result = 0;
    while (q--) {
        unsigned int l = nextRand17();
        unsigned int r = nextRand17();
        if (l > r) swap(l, r);
        unsigned int x = nextRand24();
        unsigned int y = nextRand24();
        if (x > y) swap(x, y);

        vector<unsigned int> p;
        find(p, f, x, y, 0);
        unsigned int c = 0;
        for (unsigned int i : p) {
            c += ub(f[i].second, r) - lb(f[i].second, l);
        }
        b += c;
        result += c;
    }
    cout << result << "\n";
}
