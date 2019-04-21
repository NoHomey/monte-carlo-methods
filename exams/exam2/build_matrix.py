from zero_matrix import zero_matrix

def build_matrix(ns, ps):
    n = len(ns)
    m = zero_matrix(n)
    for i, neibs in enumerate(ns):
        for j in range(4):
            if neibs[j] is not None:
                m[i][neibs[j]] = ps[j]
    for i in range(n):
        s = 0
        for j in range(n):
            if j != i:
                s += m[i][j]
        m[i][i] = 1 - s
    return m