#include <iostream>
#include <vector>
#define M 64
#define N 60
#define K 32

#define A 0		// addres a[0]
#define B 2048	// addres b[0] = M * K
#define C 5888	// addres c[0] = M * K + 2 * N * K because b is 16 bit

using namespace std;

int main() {
	vector<vector<int>> tags(2, vector<int>(16));
	vector<vector<vector<int>>> flags(2, vector<vector<int>>(16, vector<int>(2, 0))); // flags[i][j][0] - VALID, flags[i][j][1] - DIRTY
	vector<int> fw(16, 0); // last block for LRU

	int cnt = 0, hit = 0, miss = 0;

	int pa = A;
	cnt++;						// init pa

	int pc = C;
	cnt++;						// init pc

	int y = 0;
	cnt++;						// init y
	for (; y < M; y++) {
		cnt++;					// tact for loop
		int x = 0;
		cnt++;					// init x
		for (; x < N; x++) {
			cnt++;				// tact for loop
			int pb = B;
			cnt++;				// init pb

			//int s = 0;
			cnt++;				// init s

			int k = 0;
			cnt++;				// init k
			for (; k < K; k++) {
				cnt++;			// tact for loop

				//s += pa[k] * pb[x];
				int paSet = (( pa + k ) >> 5) & 0xf; // 4 bits of set
				int paTag = (( pa + k ) >> 9) & 0x7ff; // 11 bits of tag
				if ( flags[0][paSet][0] == 1 && tags[0][paSet] == paTag ) {
					cnt += 6;
					hit++;
					fw[paSet] = 1;
				} else if ( flags[1][paSet][0] == 1 && tags[1][paSet] == paTag ) {
					cnt += 6;
					hit++;
					fw[paSet] = 0;
				} else {
					miss++;
					if (flags[fw[paSet]][paSet][1] == 1)
						cnt += 116; // 100 response time + 16 writing time
					cnt += 120; // 100 response mem time + 16 reading time + 4 response cache time
					tags[fw[paSet]][paSet] = paTag;
					flags[fw[paSet]][paSet][0] = 1;
					flags[fw[paSet]][paSet][1] = 0;
					fw[paSet] = !fw[paSet];
				}

				int pbSet = (( pb + 2 * x ) >> 5) & 0xf; // 4 bits of set
				int pbTag = (( pb + 2 * x ) >> 9) & 0x7ff; // 11 bits of tag
				if ( flags[0][pbSet][0] == 1 && tags[0][pbSet] == pbTag ) {
					cnt += 6;
					hit++;
					fw[pbSet] = 1;
				} else if ( flags[1][pbSet][0] == 1 && tags[1][pbSet] == pbTag ) {
					cnt += 6;
					hit++;
					fw[pbSet] = 0;
				} else {
					miss++;
					if (flags[fw[pbSet]][pbSet][1] == 1)
						cnt += 116; // 100 response time + 16 writing time
					cnt += 120; // 100 response mem time + 16 reading time + 4 response cache time
					tags[fw[pbSet]][pbSet] = pbTag;
					flags[fw[pbSet]][pbSet][0] = 1;
					flags[fw[pbSet]][pbSet][1] = 0;
					fw[pbSet] = !fw[pbSet];
				}

				cnt += 7; // operators +, =, *

				pb += 2 * N;
				cnt += 2; // for operators + and =
			}

			//pc[x] = s;

			int pcSet = (( pc + 4 * x ) >> 5) & 0xf; // 4 bits of set
			int pcTag = (( pc + 4 * x ) >> 9) & 0x7ff; // 11 bits of tag
			if ( flags[0][pcSet][0] == 1 && tags[0][pcSet] == pcTag ) {
				cnt += 6;
				hit++;
				fw[pcSet] = 1;
			} else if ( flags[1][pcSet][0] == 1 && tags[1][pcSet] == pcTag ) {
				cnt += 6;
				hit++;
				fw[pcSet] = 0;
			} else {
				miss++;
				if (flags[fw[pcSet]][pcSet][1] == 1)
					cnt += 116; // 100 response time + 16 writing time
				cnt += 120; // 100 response mem time + 16 reading time + 4 response cache time
				tags[fw[pcSet]][pcSet] = pcTag;
				flags[fw[pcSet]][pcSet][0] = 1;
				flags[fw[pcSet]][pcSet][1] = 1;
				fw[pcSet] = !fw[pcSet];
			}
		}
		pa += K;
		cnt += 2; // for operators + and =

		pc += 4 * N;
		cnt += 2; // for operators + and =
	}
	cout << cnt << " " << hit << " " << miss << "\n";
}