#include <bits/stdc++.h>

using namespace std;

int main() {
	int n, k;
	fstream in("muggers.in", fstream::in), out("muggers.out", fstream::out);
	in >> n >> k;

	vector<int> dp(n + 1);
	dp[0] = 0;
	for (int i = 1; i <= n; i++) {
		int c;
		in >> c;
		int m = 1e9;
		for (int j = i - 1; j >= 0 && i - j <= k + 1; j--) {
			if (dp[j] < m)
				m = dp[j];
		}
		dp[i] = m + c;
	}
	out << dp[n] << "\n";
}
