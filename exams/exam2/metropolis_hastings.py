from zero_matrix import zero_matrix
from is_zero import is_zero

def metropolis_hastings(p, q):
    n = len(p)
    a = zero_matrix(n)
    m = zero_matrix(n)
    for i, row in enumerate(q):
        for j, e in enumerate(row):
            if is_zero(e):
                a[i][j] = 1
            else:
                a[i][j] = min(1.0, (p[j] * q[j][i]) / (p[i] * e))
    for i, row in enumerate(a):
        for j, e in enumerate(row):
            if i != j:
                m[i][j] = q[i][j] * e
    for i, row in enumerate(a):
        s = 0
        for k, e in enumerate(row):
            if k != i:
                s += q[i][k] * e
        m[i][i] = 1 - s
    return m