#include <bits/stdc++.h>

using namespace std;

int main() {
	int n;
	cin >> n;
	vector<int> a(n);
	for (int i = 0; i < n; i++)
		cin >> a[i];
	sort(a.begin(), a.end());
	int m = -1;
	if (a[0] > 0) m = 0;
	for (int i = 0; i < n-1 && m == -1; i++)
		if (a[i+1] - a[i] > 1) m = a[i] + 1;

	if (m == -1) m = a[n-1] + 1;
	cout << m << "\n";
}