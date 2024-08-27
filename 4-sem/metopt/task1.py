import numpy as np

def newton(begin_x, eps, func, grad, hessian):
    last_x = np.array([None])
    while True:
        grad_val = grad(begin_x)
        hessian_inv_val = np.linalg.inv(hessian(begin_x))
        next_x = np.array(begin_x - (grad_val * hessian_inv_val))[0]
        a = np.linalg.norm(next_x - begin_x)
        if (last_x != None).all() and (a / abs(1 - a/np.linalg.norm(begin_x - last_x))) < eps:
            return next_x
        else:
            last_x = begin_x
            begin_x = next_x


f = lambda x: (1 - x[0])**2 + 100 * (x[1] - x[0]**2)**2

lf = lambda x: f(x) - x[2] * (x[0]**2 + x[1]) 

dfdx = lambda x: -2 * (1 - x[0]) - 400 * x[0] * (x[1] - x[0]**2) - 2 * x[2] * x[0]
dfdy = lambda x: 200 * (x[1] - x[0]**2) - x[2]
dfdl = lambda x: -x[0]**2 - x[1]

gradf = lambda x: np.array([dfdx(x), dfdy(x), dfdl(x)])

dfdxdx = lambda x: 2 - 400 * x[1] + 1200 * x[0]**2 - 2 * x[2]
dfdxdy = lambda x: -400 * x[0]
dfdydy = lambda x: 200
dfdldl = lambda x: 0
dfdxdl = lambda x: -2 * x[0]
dfdydl = lambda x: -1

hessf = lambda x: np.asmatrix(np.array([[dfdxdx(x), dfdxdy(x), dfdxdl(x)], [dfdxdy(x), dfdydy(x), dfdydl(x)], [dfdxdl(x), dfdydl(x), dfdldl(x)]]))

print("Enter starting point:")
begin = np.array([float(x) for x in input().split()])

res = newton(begin, 0.00001, f, gradf, hessf)

print("x =", res[0], " y =", res[1], "f(x, y) =", f(res))

