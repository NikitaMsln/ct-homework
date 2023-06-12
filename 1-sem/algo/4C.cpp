#include <bits/stdc++.h>

using namespace std;

int compress(vector<int>& p, int i) {
	vector<int> v;
	while (p[i] != i) {
		v.push_back(i);
		i = p[i];
	}
	for (int j = 0; j < v.size(); j++)
		p[v[j]] = i;
	return i;
}

int main() {
	int n;
	cin >> n;
	vector<int> p(n), c(n);
	for (int i = 0; i < n; i++) {
		p[i] = i;
		cin >> c[i];
	}

	for (int i = 0; i < n; i++) {
		int j = compress(p, c[i] - 1);
		p[j] = (j + 1 + n) % n;
		cout << j + 1 << " ";
	}
	cout << "\n";
}
