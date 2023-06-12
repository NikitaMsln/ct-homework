#include <bits/stdc++.h>

using namespace std;

int main() {
	int n;
	cin >> n;
	vector<int> l(10, 0);
	vector<vector<int>> a(10, vector<int>());
	for (int i = 0; i < n; i++) {
		int v;
		cin >> v;
		a[v%10].push_back(v);
		l[v%10]++;
	}
	for (int i = 0; i < 10; i++)
		for (int j = 0; j < l[i]; j++)
			cout << a[i][j] << " ";
	cout << "\n";
}