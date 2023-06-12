#include <bits/stdc++.h>

using namespace std;

bool check(vector<int>& a, int n, int k, int d) {
	int l = a[0];
	k--;
	for (int i = 0; i < n && k > 0; i++) {
		if (a[i] - l >= d) {
			l = a[i];
			k--;
		}
	}
	return k <= 0;
}

int main() {
	int n, k;
	cin >> n >> k;
	vector<int> a(n);
	for (int i = 0; i < n; i++)
		cin >> a[i];

	int l = 1, r = (a[n-1] - a[0]) / (k - 1) + 1;

	while (l < r - 1) {
		int s = (l + r) / 2;
		if (check(a, n, k, s))
			l = s;
		else
			r = s;
	}
	cout << l << "\n";
}
