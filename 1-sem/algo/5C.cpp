#include <bits/stdc++.h>

using namespace std;

int main() {
	int n, M;
	cin >> n >> M;
	vector<int> m(n), c(n);

	for (int i = 0; i < n; i++) cin >> m[i];
	for (int i = 0; i < n; i++) cin >> c[i];

	vector<vector<int>> dp(n + 1, vector<int>(M+1, 0));
	vector<vector<bool>> dpu(n + 1, vector<bool>(M+1, 0));

	for (int i = 1; i <= n; i++) {
		for (int j = 0; j <= M; j++) {
			if (m[i - 1] > j) {
				dp[i][j] = dp[i-1][j];
				dpu[i][j] = 0;
			} else {
				int x = dp[i-1][j-m[i-1]] + c[i-1];
				if (dp[i-1][j] > x) {
					dp[i][j] = dp[i-1][j];
					dpu[i][j] = 0;
				} else {
					dp[i][j] = x;
					dpu[i][j] = 1;
				}
			}
		}
	}
	int resI = -1, maxV = 0;
	for (int i = 1; i <= n; i++) {
		if (maxV < dp[i][M]) {
			resI = i;
			maxV = dp[i][M];
		}
	}

	vector<int> res;
	int cnt = 0;
	while (resI > 0) {
		if (dpu[resI][M]) {
			res.push_back(resI);
			cnt++;
			M -= m[resI-1];
		}
		resI--;
	}

	cout << cnt << "\n";
	for (int i = cnt-1; i >= 0; i--)
		cout << res[i] << " ";
	cout << "\n";

}
