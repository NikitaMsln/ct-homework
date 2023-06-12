#include <bits/stdc++.h>

using namespace std;

bool check(vector<int>& a, int n, int k, int l) {
	if (l == 0) return true;
	for (int i = 0; i < n; i++)
		k -= a[i] / l;
	return k <= 0;
}

int main() {
	int n, k;
	cin >> n >> k;
	vector<int> a(n);
	for (int i = 0; i < n; i++)
		cin >> a[i];
	int l = 0, r = 1e7 + 1;
	while (l < r - 1) {
		int s = (l + r) / 2;
		if (check(a, n, k, s)) {
			l = s;
		} else {
			r = s;
		}
	}
	cout << l << "\n";
}
