import scipy
import numpy as np
import matplotlib.pyplot as plt

class my_rv_continuous(scipy.stats.rv_continuous):
    def _pdf(self, x):
        return 3 * (x ** 2) * np.exp( - (x ** 3)) if x >= 0 else 0

rv = my_rv_continuous(a=0, b=1000, name="my")

n = int(input())

res = rv.rvs(size=n)

count, bins, ignored = plt.hist(res, min(50, n), density=True)
plt.plot(bins, 3 * (bins ** 2) *  np.exp( - (bins ** 3)),
         linewidth=2, color='r')
plt.show()
