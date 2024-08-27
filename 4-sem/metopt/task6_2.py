from task6_1 import simplex_method


c = [float(x) for x in input().split()]
n = int(input())
A = []
b = []
for i in range(n):
    p = [float(x) for x in input().split()]
    A.append(p[:-1])
    b.append(p[-1])

vect, val = simplex_method(c, A, b)
while True:
    max_d = 0
    max_i = -1
    for i in range(len(vect)):
        d = 0
        if vect[i] >= 1:
            d = vect[i] - 1
        else:
            d = vect[i]
        if d > max_d:
            max_d = d
            max_i = i
    if max_i == -1:
        break

    na = [0 for i in range(len(c))]
    na[max_i] = 1
    A.append(na)
    nb = 0
    if vect[max_i] >= 1:
        nb = 1
    b.append(nb)
    vect, val = simplex_method(c, A, b)

print(vect, val)
