import scipy
import numpy as np
import matplotlib.pyplot as plt

eps = 0.01
delta = 0.05

p = float(input())

varX = (1 - p) / (p ** 2)

n = int(varX / (delta * (eps ** 2)))
print(n)

cur = 0
result = []

for i in range(100):
    res = np.sum(np.random.geometric(p=p, size=n))
    curr = res / n - (1 / p)
    result.append(curr)
    if curr < eps:
        cur += 1

print(cur, cur / 100)
plt.hist(np.array(result), 50, density=True)
plt.show()
