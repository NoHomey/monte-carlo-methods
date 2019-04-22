from build_matrix import build_matrix
from metropolis_hastings import metropolis_hastings
from stat_dist import stat_dist
from rejection_rates import rejection_rates
from roundf import roundf

def printMatrix(m):
    for row in m:
        print([roundf(x) for x in row])

# right, up, left, down
ns = [ [None, 1   , None, None]
     , [3   , 2   , None, 0   ]
     , [None, None, None, 1   ]
     , [4   , None, 1   , None]
     , [None, 5   , 3   , None]
     , [None, None, None, 4   ]
     ]

ps = [7, 5, 4, 1, 5, 7]

# right, up, left, down
qM = build_matrix(ns, [0.25, 0.25, 0.25, 0.25])

# right, up, left, down
qMH = build_matrix(ns, [0.1, 0.1, 0.7, 0.1])

m = metropolis_hastings(ps, qM)

mh = metropolis_hastings(ps, qMH)

print("metropolis:")
printMatrix(m)
print()
print("metropolis-hasting:")
printMatrix(mh)
print()
print("rejection rates metropolis:", rejection_rates(m))
print()
print("rejection rates metropolis-hastings:", rejection_rates(mh))
print()
print("distribution:", stat_dist(ps))