#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

int main() {
	int n;
	cin >> n;
	vector<long long> a(n), b(n);
	long long s = 0;
	for (int i = 0; i < n; i++) {
		cin >> a[i] >> b[i];
		s += a[i];
	}
	sort(a.begin(), a.end());
	sort(b.begin(), b.end());
	long long ai = 1, bi = 0, resv = s, lv = a[0];
	long long res = s;
	while (ai < n || bi < n) {
		if (ai >= n || a[ai] > b[bi]) {
			s += (b[bi] - lv) * (ai - bi);
			if (s > res) {
				res = s;
				resv = b[bi];
			}
			lv = b[bi];
			s -= b[bi];
			bi++;
		}
		else {
			s += (a[ai] - lv) * (ai - bi);
			if (s > res) {
				res = s;
				resv = a[ai];
			}
			lv = a[ai];
			ai++;
		}
	}
	cout << resv << " " << res << "\n";
}