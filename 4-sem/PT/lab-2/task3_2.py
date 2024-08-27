import scipy
import numpy as np
import matplotlib.pyplot as plt

def gen_by_inverse_F(size=1):
    res = np.random.uniform(0, 1, size)
    return (- np.log(1 - res)) ** (1/3)

n = int(input())

res = gen_by_inverse_F(size=n)

count, bins, ignored = plt.hist(res, min(50, n), density=True)
plt.plot(bins, 3 * (bins ** 2) *  np.exp( - (bins ** 3)),
         linewidth=2, color='r')
plt.show()
