from is_zero import is_zero 
from numpy import random

def prob_info(m):
    info = [[] for i in range(len(m))]
    for i, row in enumerate(m):
        for j, p in enumerate(row):
            if not is_zero(p):
                info[i].append((j, p))
    return info

def rejection_rates(m):
    info = prob_info(m)
    n = len(m)
    history = [[0, 0] for i in range(n)]
    state = random.randint(n)
    for i in range(10000000):
        j = random.randint(len(info[state]))
        a = random.uniform()
        x, p = info[state][j]
        history[x][1] += 1
        if a < p:
            state = x
            history[x][0] += 1
    return [round((t - a) / t, 3) for [a, t] in history]