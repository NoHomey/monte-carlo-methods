def stat_dist(ps):
    s = 0
    for p in ps:
        s += p
    return [round(p / s, 3) for p in ps]