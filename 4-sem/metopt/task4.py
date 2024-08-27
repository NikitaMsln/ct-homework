import numpy as np
import scipy
import math

def any_nan(arr):
    return np.array([math.isnan(t) for t in arr]).any()

def gradient_d(begin_x, grad_eps, step, func, grad):
    grad_x = grad(begin_x)
    while (True):
        grad_val = grad_x
        d = step * grad_val

        next_x = begin_x - d
        grad_x = grad(next_x)

        if np.linalg.norm(grad_x) < grad_eps:
            return next_x

        if math.isnan(step) or any_nan(next_x) or any_nan(grad_x):
            print("Gradient descent is out of limits")
            return begin_x
        begin_x = next_x

def to_basis(size, matrix):
    if len(matrix) == 0:
        res = [[0] * size] * size
        for i in range(size):
            res[i][i] = 1
        return res

    res = [[matrix[0][0]]]
    added = 0
    while len(res) < size:
        for i in range(len(res)):
            if i < len(matrix):
                res[i].append(matrix[i][len(res[i])])
            else:
                res[i].append(0)
        if len(res) < len(matrix):
            res.append(matrix[len(res)][:len(res[0])])
        else:
            res.append([0] * len(res[0]))
            res[-1][added] = 1
            added += 1
            while abs(np.linalg.det(res)) < 0.000001:
                res[-1][added - 1] = 0
                res[-1][added] = 1
                added += 1
    return res

def gram_schmidt_process(matrix):
    res = [matrix[0]]
    for i in range(1, len(matrix)):
        res.append(matrix[i] + [])
        for j in range(0, i):
            c = np.sum(np.array(matrix[i]) * np.array(res[j])) / np.linalg.norm(res[j])
            for k in range(0, len(res[0])):
                res[i][k] -= c * res[j][k]
    return res
    

def linear_gradient_d(size, cond, grad_eps, step, func, grad):
    # find one solution
    matrix = to_basis(size, cond[0])
    b = cond[1] + []
    while len(b) < size:
        b.append(0)
    
    one_solution = np.linalg.solve(matrix, b)
    
    # find projection
    matrix = cond[0] + []
    while len(matrix) < size:
        matrix.append([0] * size)

    ker = scipy.linalg.null_space(matrix)

    # transpose, to_basis, gram-schidt, transpose
    ker = [list(x) for x in zip(*ker)]
    
    n = len(ker)
    
    ker = to_basis(size, ker)
    ker = gram_schmidt_process(ker)
    ker_t = [list(x) for x in zip(*ker)]

    def project(size, mat, mat_t, vector):
        solve = np.linalg.solve(mat_t, vector)
        res = [0] * len(vector)
        for i in range(size):
            for j in range(len(matrix)):
                res[j] += solve[i] * mat[i][j]
        return np.array(res)

    projected_grad = lambda x: project(n, ker, ker_t, grad(x)) 

    return gradient_d(one_solution, grad_eps, step, func, projected_grad)
    

f = lambda x: (1 - x[0])**2 + 100 * (x[1] - x[0]**2)**2

dfdx = lambda x: -2 * (1 - x[0]) - 400 * x[0] * (x[1] - x[0]**2)
dfdy = lambda x: 200 * (x[1] - x[0]**2)

gradf = lambda x: np.array([dfdx(x), dfdy(x)])

g = lambda x: x[0]**2 + x[1]**2 + x[2]**2

gradg = lambda x: np.array([2 * x[0], 2 * x[1], 2 * x[2]])

res = linear_gradient_d(2, ([[-1, 1]], [3]), 0.00001, 0.001, f, gradf)

res = linear_gradient_d(3, ([[1, 1, 1], [2, -3, 4]], [4, -10]), 0.00001, 0.001, g, gradg)

print("x =", res[0], "y =", res[1], "f(x, y) =", f(res))
print(res, g(res))
print("y - x =", res[1] - res[0])
print("y + x + z =", res[0] + res[1] + res[2])
print("2x - 3y + 4z =", 2 * res[0] - 3 * res[1] + 4 * res[2])
