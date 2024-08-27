import math as m
import fractions as fr

def probabilityTr(n : int, k : int, p : fr.Fraction) -> fr.Fraction:
    return (m.factorial(n) // (m.factorial(k) * m.factorial(n - k))) * (p ** k) * ((1 - p) ** (n - k))

def probabilityPaus(n : int, k : int, p : fr.Fraction) -> fr.Fraction:
    return (n * p) ** k / m.factorial(k) * fr.Fraction(m.e) ** (- n * p)

def probabilityLocalML(n : int, k : int, p : fr.Fraction) -> fr.Fraction:
    return m.exp(-((k - n * p)**2/(2 * n * p * (1 - p)))) / m.sqrt(2 * fr.Fraction(m.pi) * n * p * (1 - p))

def probabilityIntML(n : int, k : int, p : fr.Fraction) -> fr.Fraction:
    res = fr.Fraction(0)
    left = fr.Fraction((k - n * p) / m.sqrt(n * p * (1 - p)) - 0.0001)
    right = fr.Fraction((k - n * p) / m.sqrt(n * p * (1 - p)) + 0.0001)
    dx = (right - left) / 10000
    while left < right:
        res += dx * m.exp(-(left ** 2 / 2)) / m.sqrt(2 * fr.Fraction(pi))
        left += dx
    return res


# Answer 1 P(S_n in [n / 2 - sqrt(npq), n / 2 + sqrt(npq)])
def ans1(calcul, n : int, p : fr.Fraction) -> fr.Fraction:
    left_b = n / 2 - m.sqrt(n * p * (1 - p))
    right_b = n / 2 + m.sqrt(n * p * (1 - p))
    left_bk = m.ceil(left_b)
    right_bk = m.ceil(right_b)
    result = 0
    for k in range(left_bk, right_bk):
        result += calcul(n, k, p)
    return result

# Answer 2 P(S_n <= 5)
def ans2(calcul, n : int, p : fr.Fraction) -> fr.Fraction:
    result = 0
    for k in range(6):
        result += calcul(n, k, p)
    return result

# Answer 3 P(S_n == k*)
def ans3(calcul, n : int, p : fr.Fraction) -> fr.Fraction:
    most_prob_k = m.ceil(n * p + p)
    return calcul(n, most_prob_k, p)

n = [100, 1000, 10000]
p = [fr.Fraction(1, 1000), fr.Fraction(1, 100), fr.Fraction(1, 10), fr.Fraction(1, 4), fr.Fraction(1, 2)]

for ni in n:
    for pi in p:
        a1tr = ans1(probabilityTr, ni, pi)
        a2tr = ans2(probabilityTr, ni, pi)
        a3tr = ans3(probabilityTr, ni, pi)
        a1ps = ans1(probabilityPaus, ni, pi)
        a2ps = ans2(probabilityPaus, ni, pi)
        a3ps = ans3(probabilityPaus, ni, pi)
        a1loc = ans1(probabilityLocalML, ni, pi)
        a2loc = ans2(probabilityLocalML, ni, pi)
        a3loc = ans3(probabilityLocalML, ni, pi)
        a1int = ans1(probabilityIntML, ni, pi)
        a2int = ans2(probabilityIntML, ni, pi)
        a3int = ans3(probabilityIntML, ni, pi)
        print("$n = " + str(ni) + ", p = " + str(float(pi)) + "$:")
        print()
        print("$p1 = (" + str(float(a1tr)) + ", " +  str(float(a2tr)) + ", " + str(float(a3tr)) + ")$")
        print()
        print("$p2 = (" + str(float(a1ps)) + ", " +  str(float(a2ps)) + ", " + str(float(a3ps)) + ")$")
        print()
        print("$d2 = (" + str((abs(float(a1ps) - float(a1tr)))) + ", " +  str((abs(float(a2ps) - float(a2tr)))) + ", " + str((abs(float(a3ps) - float(a3tr)))) + ")$")
        print()
        print("$p3 = (" + str(float(a1loc)) + ", " +  str(float(a2loc)) + ", " + str(float(a3loc)) + ")$")
        print()
        print("$d3 = (" + str((abs(float(a1loc) - float(a1tr)))) + ", " +  str((abs(float(a2loc) - float(a2tr)))) + ", " + str((abs(float(a3loc) - float(a3tr)))) + ")$")
        print()
        print("$p4 = (" + str(float(a1int)) + ", " +  str(float(a2int)) + ", " + str(float(a3int)) + ")$")
        print()
        print("$d4 = (" + str((abs(float(a1int) - float(a1tr)))) + ", " +  str((abs(float(a2int) - float(a2tr)))) + ", " + str((abs(float(a3int) - float(a3tr)))) + ")$")
        print()
        print()
