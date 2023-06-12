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
	int n, m, k;
	cin >> n >> m >> k;
	vector<int> ptr(n, -1), v(n, 0);

	for (int i = 0; i < m; i++) {
		int p;
		cin >> p >> p;
	}
	vector<string> s(k);
	vector<int> v1(k), v2(k);
	for (int i = 0; i < k; i++)
		cin >> s[i] >> v1[i] >> v2[i];
	vector<bool> res;
	for (int i = k - 1; i >= 0; i--) {
		if (s[i] == "cut") {
			int p1, p2;
			p1 = compress(ptr, v, v1[i] - 1);
			p2 = compress(ptr, v, v2[i] - 1);
			if (p1 != p2) {
				ptr[p1] = p2;
			}
		} else {
			int p1, p2;
			p1 = compress(ptr, v, v1[i] - 1);
			p2 = compress(ptr, v, v2[i] - 1);
			res.push_back(p1 == p2);
		}
	}
	for (int i = res.size() - 1; i >= 0; i--)
		cout << ((res[i])? "YES" : "NO") << "\n";

}
