#define _CRT_SECURE_NO_WARNINGS
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#ifdef _MSC_VER
#include <intrin.h>
#else
#include <ia32intrin.h>
#endif

void RGB2YUV(const uint8_t *in, uint8_t *restrict out, size_t width, size_t height, ptrdiff_t in_stride, ptrdiff_t out_stride);
void YUV2RGB(const uint8_t *in, uint8_t *restrict out, size_t width, size_t height, ptrdiff_t in_stride, ptrdiff_t out_stride);

int main(void)
{
	size_t width, height, size;
	char c;

	FILE *f = fopen("test_data/rgb.pnm", "rb");
	if (!f)
		return -1;
	if (fscanf(f, "P6 %zu %zu 255%c", &width, &height, &c) != 3 || !width || !height || c != '\n')
		return -2;
	size = width*height;
	uint8_t *rgb = malloc(size*3);
	if (!rgb)
		return -3;
	if (fread(rgb, size*3, 1, f) != 1)
		return -2;
	fclose(f);

	f = fopen("test_data/yuv.pnm", "rb");
	if (!f)
		return -1;
	size_t width2, height2;
	if (fscanf(f, "P6 %zu %zu 255%c", &width2, &height2, &c) != 3 || width != width2 || height != height2 || c != '\n')
		return -2;
	uint8_t *yuv = malloc(size*3);
	if (!yuv)
		return -3;
	if (fread(yuv, size*3, 1, f) != 1)
		return -2;
	fclose(f);

	uint8_t *buf = malloc(size*4);
	if (!buf)
		return -3;

	RGB2YUV(rgb + size*3 - width*3, buf + size*4 - width*4, width, height, -(ptrdiff_t)width*3, -(ptrdiff_t)width*4);

	uint8_t delta_0 = 0, delta_1 = 0, delta_2 = 0;
	for (size_t y = 0; y < height; y++)
		for (size_t x = 0; x < width; x++)
		{
			uint8_t d0 = abs(buf[width*4*y + x*4    ] - yuv[width*3*y + x*3    ]);
			uint8_t d1 = abs(buf[width*4*y + x*4 + 1] - yuv[width*3*y + x*3 + 1]);
			uint8_t d2 = abs(buf[width*4*y + x*4 + 2] - yuv[width*3*y + x*3 + 2]);
			delta_0 = delta_0 < d0 ? d0 : delta_0;
			delta_1 = delta_1 < d1 ? d1 : delta_1;
			delta_2 = delta_2 < d2 ? d2 : delta_2;
		}
	printf("Max delta RGB->YUV:\nY: %u\nU: %u\nV: %u\n", delta_0, delta_1, delta_2);
	unsigned int dummy;
	volatile uint64_t tr = __rdtscp(&dummy);
	for (unsigned int i = 0; i < 10000; i++)
		RGB2YUV(rgb, buf, width, height, width*3, width*4);
	tr = __rdtscp(&dummy) - tr;
	printf("Time: %llu\n", tr / 1000000);

	for (size_t y = 0; y < height; y++)
		for (size_t x = 0; x < width; x++)
		{
			buf[width*4*y + x*4    ] = yuv[width*3*y + x*3    ];
			buf[width*4*y + x*4 + 1] = yuv[width*3*y + x*3 + 1];
			buf[width*4*y + x*4 + 2] = yuv[width*3*y + x*3 + 2];
			buf[width*4*y + x*4 + 3] = 0xA5;
		}

	YUV2RGB(buf + size*4 - width*4, yuv + size*3 - width*3, width, height, -(ptrdiff_t)width*4, -(ptrdiff_t)width*3);

	delta_0 = 0; delta_1 = 0; delta_2 = 0;
	for (size_t y = 0; y < height; y++)
		for (size_t x = 0; x < width; x++)
		{
			uint8_t d0 = abs(yuv[width*3*y + x*3    ] - rgb[width*3*y + x*3    ]);
			uint8_t d1 = abs(yuv[width*3*y + x*3 + 1] - rgb[width*3*y + x*3 + 1]);
			uint8_t d2 = abs(yuv[width*3*y + x*3 + 2] - rgb[width*3*y + x*3 + 2]);
			delta_0 = delta_0 < d0 ? d0 : delta_0;
			delta_1 = delta_1 < d1 ? d1 : delta_1;
			delta_2 = delta_2 < d2 ? d2 : delta_2;
		}

	printf("\nMax delta YUV->RGB:\nR: %u\nG: %u\nB: %u\n", delta_0, delta_1, delta_2);
	tr = __rdtscp(&dummy);
	for (unsigned int i = 0; i < 10000; i++)
		YUV2RGB(buf, yuv, width, height, width*4, width*3);
	tr = __rdtscp(&dummy) - tr;
	printf("Time: %llu\n", tr / 1000000);

	free(buf);
	free(yuv);
	free(rgb);

	return 0;
}
