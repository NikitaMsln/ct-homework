#include <bits/stdc++.h>

using namespace std;

int main() {
	int n, m;
	cin >> n >> m;
	vector<vector<int>> dp(n, vector<int>(m));
	vector<vector<bool>> v(n, vector<bool>(m, 0));

	v[0][0] = 1;
	for (int i = 0; i < n; i++) {
		for (int j = 0; j < m; j++) {
			int m = 0;
			if (i >= 2 && j >= 1 && m < dp[i - 2][j - 1] && v[i - 2][j - 1]) {
				m = dp[i - 2][j - 1];
				v[i][j] = 1;
			}
			if (i >= 1 && j >= 2 && v[i - 1][j - 2]) {
				if (dp[i - 1][j - 2] > m) m = dp[i - 1][j - 2];
				v[i][j] = 1;
			}

			int v;
			cin >> v;

			dp[i][j] = m + v;
		}
	}
	if (v[n - 1][m - 1])
		cout << dp[n - 1][m - 1] << "\n";
	else
		cout << "-1\n";
}
