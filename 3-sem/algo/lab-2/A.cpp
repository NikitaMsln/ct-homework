#include <bits/stdc++.h>
 
using namespace std;
 
int main() {
    int n;
    cin >> n;
    vector<vector<int>> d(n, vector<int>(n));
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            cin >> d[i][j];
        }
    }
 
    for (int i = 0; i < n; i++ ) {
        for (int u = 0; u < n; u++) {
            for (int v = 0; v < n; v++) {
                if (d[u][i] + d[i][v] < d[u][v]) {
                    d[u][v] = d[u][i] + d[i][v];
                }
            }
        }
    }
 
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            cout << d[i][j] << " ";
        }
        cout << "\n";
    }
 
}