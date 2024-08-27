def canonical_simplex_method(c, A, b, start_basis):
    B = []
    for i in range(len(c) + 1):
        B.append([])
        for j in range(len(A)):
            B[-1].append(0)

    for i in range(len(b)):
        B[-1][i] = b[i]

    for i in range(len(A)):
        for j in range(len(A[i])):
            B[j][i] = A[i][j]
    
    basis = start_basis + []
    while True:
        max_i_v = 0
        max_i = -1
        for i in range(len(A[0])):
            delta = 0
            for j in range(len(basis)):
                delta += c[basis[j]] * B[i][j]
            delta -= c[i]
            if delta > max_i_v:
                max_i_v = delta
                max_i = i
        
        if max_i == -1:
            break
        
        min_j_v = 1e18
        min_j = -1
        for j in range(len(B[-1])):
            if B[max_i][j] > 0 and min_j_v > B[-1][j] / B[max_i][j]:
                min_j_v = B[-1][j] / B[max_i][j]
                min_j = j

        if min_j < 0:
            break
        basis[min_j] = max_i

        cf = B[max_i][min_j]
        for i in range(len(B)):
            B[i][min_j] = B[i][min_j] / cf

        for i in range(len(B)):
            if i == max_i:
                continue
            for j in range(len(B[i])):
                if j == min_j:
                    continue
                B[i][j] = B[i][j] - B[max_i][j] * B[i][min_j]
        
        for j in range(len(B[max_i])):
            if j != min_j:
                B[max_i][j] = 0
    
    res_vector = [0 for _ in range(len(c))]
    res_value = 0
    for i in range(len(basis)):
        res_vector[basis[i]] = B[-1][i]
        res_value += res_vector[basis[i]] * c[basis[i]]

    return res_vector, res_value


# maximize c^t * x with Ax <= b
def simplex_method(c, A, b):
    A_c = []
    for i in range(len(A)):
        A_c.append(A[i].copy())

    c_c = [-x for x in c]
    
    basis = []
    for i in range(len(b)):
        basis.append(len(c) + i)
        c_c.append(0)
        for j in range(len(A)):
            if i == j:
                A_c[j].append(1)
            else:
                A_c[j].append(0)

    vect, val = canonical_simplex_method(c_c, A_c, b, basis)
    val *= -1
    return vect[:len(c)], val

if __name__ == "__main__":
    # принимается c - вектор такой, что c_1 * x_1 + ... + c_n * x_n -> max
    # далее матрица A и b такие, что Ax <= b
    # например для x + 2y -> max и 3x + 4y <= 10 вводится
    # 1 2
    # 1
    # 3 4 10
    
    c = [float(x) for x in input().split()]
    n = int(input())
    A = []
    b = []
    for i in range(n):
        p = [float(x) for x in input().split()]
        b.append(p[-1])
        A.append(p[:-1])

    print(simplex_method(c, A, b))

