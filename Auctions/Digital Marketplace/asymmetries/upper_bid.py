import numpy as np
import functools as fts

w = 0.5
reps = [0.25, 0.5, 0.75]

# Estimate upper bound on bids
# Define uniform CDF (for convenience)
def F(bounds, x):
  if x < bounds[0]:
    return 0.0
  elif x > bounds[1]:
    return 1.0
  else:
    return (x - bounds[0]) / (bounds[1] - bounds[0])
# Estimated cost support bounds
lower_extremities = [(1-w)*r for r in reps]
upper_extremities = [(1-w)*r + w for r in reps]
# Estimate upper bound on bids
vals = np.linspace(upper_extremities[0], upper_extremities[1], 10000)
cdfs = list(map(lambda ext: fts.partial(F, ext), zip(lower_extremities[1:], upper_extremities[1:])))
objective_func = lambda x: (x - upper_extremities[0])*fts.reduce(lambda p, r: p*r, [(1-cdf(x)) for cdf in cdfs], 1)
tabulated = [objective_func(v) for v in vals]
maximum = max(tabulated)
b_upper = min([v for v, i in zip(vals, range(len(vals))) if tabulated[i] == maximum])
print(b_upper)
