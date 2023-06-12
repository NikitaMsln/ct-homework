#include <bits/stdc++.h>

using namespace std;

int main() {
	string src, res;
	cin >> src >> res;
	int srcn = src.length(), resn = res.length();
	vector<vector<int>> dp(srcn+1, vector<int>(resn+1, 0));

	for (int i = 0; i <= srcn; i++) dp[i][0] = i;
	for (int i = 0; i <= resn; i++) dp[0][i] = i;

	for (int i = 1; i <= srcn; i++) {
		for (int j = 1; j <= resn; j++) {
			if (src[i-1] == res[j-1]) {
				dp[i][j] = dp[i-1][j-1];
			} else {
				if (dp[i-1][j] < dp[i][j-1]) {
					if (dp[i-1][j] < dp[i-1][j-1]) {
						dp[i][j] = dp[i-1][j]+1;
					} else {
						dp[i][j] = dp[i-1][j-1]+1;
					}
				} else {
					if (dp[i][j-1] < dp[i-1][j-1]) {
						dp[i][j] = dp[i][j-1]+1;
					} else {
						dp[i][j] = dp[i-1][j-1]+1;
					}
				}
			}
		}
	}

	cout << dp[srcn][resn] << "\n";
}
