import scipy
import numpy as np
import matplotlib.pyplot as plt

def density(x):
    return 3 * (x ** 2) * np.exp(- (x ** 3))

min_value = 0
max_value = density((2 / 3) ** (1 / 3))

begin = 0
end = 10

n = int(input())

res = []

for i in range(n):
    while True:
        x = np.random.uniform(begin, end, 1)[0]
        y = np.random.uniform(min_value, max_value, 1)[0]
        if (y < density(x)):
            res.append(x)
            break

res = np.asarray(res)

count, bins, ignored = plt.hist(res, min(50, n), density=True)
plt.plot(bins, 3 * (bins ** 2) *  np.exp( - (bins ** 3)),
         linewidth=2, color='r')
plt.show()
