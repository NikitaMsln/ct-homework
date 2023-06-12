#include <bits/stdc++.h>

using namespace std;

int bin_search(vector<int>& array, int size, int value, bool comp(int, int)) {
	int l = 0, r = size;
	while (l < r) {
		int s = (l + r) >> 1;
		if (comp(array[s], value))
			l = s + 1;
		else
			r = s;
	}
	return r;
}

int main() {
	int n, k;
	cin >> n;
	vector<int> a(n);
	for (int i = 0; i < n; i++)
		cin >> a[i];
	sort(a.begin(), a.end());
	cin >> k;
	while(k--) {
		int l, r;
		cin >> l >> r;
		auto lit = bin_search(a, n, l, [](int x, int y) { return x < y; }),
		     rit = bin_search(a, n, r, [](int x, int y) { return x <= y; });

		cout << rit - lit << "\n";
	}
}
