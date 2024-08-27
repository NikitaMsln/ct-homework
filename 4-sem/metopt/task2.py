import numpy as np
import math

def any_nan(arr):
    return np.array([math.isnan(t) for t in arr]).any()

def gradient_d(begin_x, grad_eps, cond, func, grad):
    grad_x = grad(begin_x)
    step = 1
    while (True):
        if (np.linalg.norm(grad_x) < grad_eps and not cond(begin_x)):
            step /= grad_eps / step
        grad_val = grad_x
        next_x = begin_x - step * grad_val
        grad_x = grad(next_x)

        if np.linalg.norm(grad_x) < grad_eps and cond(begin_x):
            return next_x

        step = abs(np.sum((next_x - begin_x) * (grad_x - grad_val))) / np.linalg.norm(grad_x - grad_val)**2
        if math.isnan(step) or any_nan(next_x) or any_nan(grad_x):
            print("Gradient descent is out of limits")
            return begin_x
        begin_x = next_x


f = lambda x: (1 - x[0])**2 + 100 * (x[1] - x[0]**2)**2
fl = lambda x: f(x) - x[2] * (x[1] + x[0]**2)

dfdx = lambda x: -2 * (1 - x[0]) - 400 * x[0] * (x[1] - x[0]**2) - 2 * x[0] * x[2]
dfdy = lambda x: 200 * (x[1] - x[0]**2) - x[2]
dfdl = lambda x: -x[0]**2 - x[1]

gradf = lambda x: np.array([dfdx(x), dfdy(x), dfdl(x)])

g = lambda x: sum([t**2 for t in gradf(x)])

dgdx = lambda x: 2 * dfdx(x) * (2 - 400 * x[1] + 1200 * x[0]**2 - 2 * x[2]) + 2 * dfdy(x) * (-400 * x[0]) + 2 * dfdl(x) * (-2 * x[0])

dgdy = lambda x: 2 * dfdx(x) * (-400 * x[0]) + 2 * dfdy(x) * 200 + 2 * dfdl(x) * (-1)

dgdl = lambda x: 2 * dfdx(x) * (-2 * x[0]) + 2 * dfdy(x) * (-1) + 2 * dfdl(x) * 0

gradg = lambda x: np.array([dgdx(x), dgdy(x), dgdl(x)])

print("Enter starting point")
begin = np.array([float(x) for x in input().split()])

res = gradient_d(begin, 1e-6, lambda x: x[0]**2 + x[1] < 0.01, g, gradg)

print("x =", res[0], "y =", res[1], "l =", res[2], "f(x, y) =", f(res), "g(x, y, l) =", g(res))

