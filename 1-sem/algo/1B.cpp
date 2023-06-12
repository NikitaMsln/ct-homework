#include <bits/stdc++.h>

using namespace std;

int main() {
	int r, c;
	cin >> r >> c;
	int n = r*c;
	vector<int> a(n);
	for (int i = 0; i < n; i++)
		cin >> a[i];
	sort(a.begin(), a.end());
	int m = 0;
	for (int i = 0; i < n; i += c)
		if (a[i + c - 1] - a[i] > m) m = a[i + c - 1] - a[i];
	cout << m << "\n";
}