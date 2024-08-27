import scipy
import numpy as np
import matplotlib.pyplot as plt

eps = 0.01
delta = 0.05

p = float(input())

varX = (1 - p) / (p ** 2)

sigma = np.sqrt(varX)

begin = 1
end = 1000000000
while end - begin > 1:
    middle = (begin + end) / 2
    res = scipy.stats.norm.cdf(np.sqrt(middle) * eps / sigma)
    if res > 1 - (delta / 2):
        end = middle
    else:
        begin = middle

n = int(end + 1)
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
