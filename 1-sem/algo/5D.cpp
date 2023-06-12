#include <bits/stdc++.h>
#define MOD (2012)

using namespace std;

int main() {
	string s;
	cin >> s;
	int n = s.length();
	vector<vector<vector<int>>> dp(2, vector<vector<int>>(n/2+1, vector<int>(n/2+1, 0)));
	dp[0][1][0] = dp[0][0][1] = 1;
	for (int i = 1; i < n; i++) {
		int now = i % 2, last = (i - 1) % 2;
		if (s[i] == '(') {
			for (int x = 0; x <= n / 2; x++) {
				for (int y = 0; y <= n / 2; y++) {
					int v1 = 0, v2 = 0;
					if (y > 0) v1 = dp[last][x][y-1];
					if (x > 0) v2 = dp[last][x-1][y];
					dp[now][x][y] = (v1 + v2) % MOD;
				}
			}
		} else {
			for (int x = n / 2; x >= 0; x--) {
				for (int y = n / 2; y >= 0; y--) {
					int v1 = 0, v2 = 0;
					if (y < n / 2) v1 = dp[last][x][y+1];
					if (x < n / 2) v2 = dp[last][x+1][y];
					dp[now][x][y] = (v1 + v2) % MOD;
				}
			}

		}
	}

	cout << dp[1][0][0] << "\n";
}
