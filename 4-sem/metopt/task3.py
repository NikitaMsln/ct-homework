import numpy as np

def l_bfgs(begin_x, history_size, eps, func, grad):
    def eval_values(x1, x2, grad):
        g1 = grad(x1)
        g2 = grad(x2)
        s = x2 - x1
        y = g2 - g1
        cof = np.sum(y * s)
        p = 1
        if (cof > eps):
            p = 1 / cof
        return (x1, g1, s, y, p)

    d = grad(begin_x)
    if (np.linalg.norm(d) < eps):
        return begin_x
    next_x = begin_x - d
    history = [eval_values(begin_x, next_x, grad)]
    begin_x = next_x

    while True:
        grad_x = grad(begin_x)
        a = []
        for i in range(len(history), 0, -1):
            a.append(history[i - 1][4] * np.sum(history[i - 1][2] * grad_x))
            grad_x = grad_x - a[-1] * history[i - 1][3]
        gamma = 1
        if (np.linalg.norm(history[-1][3]) > 0):
            gamma = np.sum(history[-1][2] * history[-1][3]) / np.linalg.norm(history[-1][3])**2
        d = gamma * grad_x
        for i in range(len(history)):
            b = history[i][4] * np.sum(history[i][3] * d)
            d = d + history[i][2] * (a[-i - 1] - b)
        next_x = begin_x - d
        history.append(eval_values(begin_x, next_x, grad))
        if (len(history) > history_size):
            history.pop(0)
        if np.linalg.norm(grad(begin_x)) < eps:
            return next_x
        begin_x = next_x

print("Enter N")
N = int(input())

def f_prot(n, val):
    res = 0
    for i in range(1, n // 2 + 1, 1):
        res += 100 * (val[2 * i - 2]**2 - val[2 * i - 1])**2 + (val[2 * i - 2] - 1)**2
    return res

def df_prot(n, val):
    res = [0] * n
    for i in range(1, n // 2 + 1, 1):
        res[2 * i - 2] = 400 * val[2 * i - 2] * (val[2 * i - 2]**2 - val[2 * i - 1]) + 2 * (val[2 * i - 2] - 1)
        res[2 * i - 1] = -200 * (val[2 * i - 2]**2 - val[2 * i - 1])
    return np.array(res)

f = lambda x: f_prot(N, x)
df = lambda x: df_prot(N, x)

print("Enter starting point")
x = np.array([float(i) for i in input().split()])
print("Enter m (history size)")
m = int(input())
res = l_bfgs(x, m, 0.00000001, f, df)
print("x = (" + ", ".join([str(x) for x in res]) + ") f(x) =", f(res))

