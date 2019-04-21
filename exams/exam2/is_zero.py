from math import isclose

def is_zero(x):
    return isclose(x, 0, abs_tol=0.00001)