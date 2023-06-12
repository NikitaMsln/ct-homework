#include <bits/stdc++.h>

using namespace std;

int compress(vector<int>& ptr, vector<int>& v, int v1) {
	vector<int> ind, dv;
	int k = 0, in = v1, s = 0;
	while (ptr[in] >= 0) {
		ind.push_back(in);
		if (k > 0)
			dv.push_back(dv[k-1] + v[in]);
		else
			dv.push_back(v[in]);
		k++;
		s += v[in];
		in = ptr[in];
	}
	for (int i = 0; i < k; i++) {
		ptr[ind[i]] = in;
		v[ind[i]] += s - dv[i];
	}
	return in;
}

int main() {
	int n, m;
	cin >> n >> m;
	vector<int> ptr(n, -1), v(n, 0);

	while (m--) {
		string s;
		cin >> s;
		if (s == "join") {
			int v1, v2;
			cin >> v1 >> v2;
			v1 = compress(ptr, v, v1 - 1);
			v2 = compress(ptr, v, v2 - 1);
			if (v1 != v2) {
				ptr[v1] = v2;
				v[v1] -= v[v2];
			}
		} else if (s == "add") {
			int v1, v2;
			cin >> v1 >> v2;
			v1 = compress(ptr, v, v1 - 1);
			v[v1] += v2;
		} else {
			int v1;
			cin >> v1;
			int p = compress(ptr, v, v1 - 1);
			if (p == v1 - 1) {
				cout << v[p] << "\n";
			} else {
				cout << v[v1 - 1] + v[p] << "\n";
			}
		}
	}
}
