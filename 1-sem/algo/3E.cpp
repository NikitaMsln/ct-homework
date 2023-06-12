#include <bits/stdc++.h>

using namespace std;

unsigned long long max_value(unsigned long long a, unsigned long long b, unsigned long long w, unsigned long long h) {

	long long n1 = (w / a) * (h / b), n2 = (w / b) * (h / a);

	if (n1 > n2)
		return n1;
	return n2;
}

int main() {
	unsigned long long n, a, b, w, h;
	cin >> n >> a >> b >> w >> h;
	unsigned long long l = 0, r = w;
	while (l < r - 1) {
		unsigned long long s = (l + r) / 2;
		if (n <= max_value(a + 2 * s, b + 2 * s, w, h))
			l = s;
		else
			r = s;
	}
	cout << l << "\n";
}
