#include <bits/stdc++.h>
 
using namespace std;
 
static int INF = INT32_MAX;
 
int main() {
    int n;
    cin >> n;
    vector<vector<int>> d(n, vector<int>(n));
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            cin >> d[i][j];
            if (d[i][j] == 100'000) {
                d[i][j] = INF;
            }
        }
    }
 
    vector<vector<int>> p(n, vector<int>(n, 0));
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            p[i][j] = j;
        }
    }
 
    int m = n;
 
    for (int i = 0; i < n; i++ ) {
        for (int u = 0; u < n; u++) {
            for (int v = 0; v < n; v++) {
                if (d[u][i] != INF && d[i][v] != INF && (d[u][i] + d[i][v] < d[u][v] || d[u][v] == INF)) {
                    d[u][v] = d[u][i] + d[i][v];
                    p[u][v] = p[u][i];
                    if (u == v && d[u][v] < 0) {
                        m = u;
                        goto end;
                    }
                }
            }
        }
    }
    end:
    if (m == n) {
        cout << "NO\n";
    } else {
        cout << "YES\n";
        vector<int> way;
        int c = m;
        do {
            way.push_back(c);
            c = p[c][m];
        } while (c != m);
        cout << way.size() << "\n";
        for (int i = 0; i < way.size(); i++) {
            cout << way[i] + 1 << " ";
        }
        cout << "\n";
    }
}