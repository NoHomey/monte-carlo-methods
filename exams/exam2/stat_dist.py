from roundf import roundf

def stat_dist(ps):
    s = 0
    for p in ps:
        s += p
    return [roundf(p / s) for p in ps]