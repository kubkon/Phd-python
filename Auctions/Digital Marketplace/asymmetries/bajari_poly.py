import functools as fts
import numpy as np
import scipy.optimize as scopt

# Scenario
w = 0.75
reps = [0.25, 0.75]
n = len(reps)
# Estimate cost support bounds
lower_extremities = [(1-w)*r for r in reps]
upper_extremities = [(1-w)*r+w for r in reps]

# Estimate upper bound on bids
# Define uniform CDF and PDF
def F(bounds, x):
  if x <= bounds[0]:
    return 0.0
  elif x >= bounds[1]:
    return 1.0
  else:
    return (x - bounds[0]) / (bounds[1] - bounds[0])
def f(bounds, x):
  if x <= bounds[0] or x >= bounds[1]:
    return 0.0
  else:
    return 1.0 / (bounds[1] - bounds[0])
# Find maximum
bs = np.linspace(upper_extremities[0], upper_extremities[1], 10000)
cdfs = list(map(lambda ext: fts.partial(F, ext), zip(lower_extremities[1:], upper_extremities[1:])))
objective_func = lambda x: (x - upper_extremities[0])*fts.reduce(lambda p, r: p*r, [(1-cdf(x)) for cdf in cdfs], 1)
tabulated = [objective_func(b) for b in bs]
maximum = max(tabulated)
b_upper = min([b for b, i in zip(bs, range(len(bs))) if tabulated[i] == maximum])

# Approximate bids using non-linear least squares
cdfs = [fts.partial(F, bounds) for bounds in zip(lower_extremities, upper_extremities)]
pdfs = [fts.partial(f, bounds) for bounds in zip(lower_extremities, upper_extremities)]
# Objective function
def func2min(params, n, K, ngrid):
  # Unpack params
  b_lower = params[0]
  ps = [[p for p in params[1+i*(K+1):(K+2)+i*(K+1)]] for i in range(n)]
  # Create grid of points
  grid = [b_lower + i*(b_upper - b_lower) / ngrid for i in range(ngrid+1)]
  # Define helper functions
  def cost_func(b_lower, ps, t):
    B = np.array([(t - b_lower)**k for k in range(K+1)])
    v = np.array(ps).T
    return b_lower + np.dot(v, B)
  def d_cost_func(b_lower, ps, t):
    bs = [0] + [(t - b_lower)**(k-1) for k in range(1, K+1)]
    B = np.array(bs)
    v = np.array(ps).T
    return np.dot(v, B)
  def g(i, b_lower, ps, t):
    p_sum = 0
    for j in [j for j in range(n) if j != i]:
      cost = cost_func(b_lower, ps[j], t)
      p_sum += d_cost_func(b_lower, ps[j], t) * pdfs[j](cost) / (1 - cdfs[j](cost))
    return 1 + p_sum * (t - cost_func(b_lower, ps[i], t))
  # Compute H
  out = 0
  for i in range(n):
    c1 = cost_func(b_lower, ps[i], b_lower)
    c2 = cost_func(b_lower, ps[i], b_upper)
    out += ngrid*(lower_extremities[i] - c1)**2 + ngrid*(b_upper - c2)**2
    for t in grid:
      out += g(i, b_lower, ps, t)**2
  return out

# Params
K = 3
ngrid = 1000
# Initial guess
x0 = [lower_extremities[1]] + [0.0 for i in range(n) for k in range(K+1)]
x0 = np.array(x0)
# Minimize
result = scopt.minimize(func2min, x0, args=(n, K, ngrid), method='nelder-mead')
print(result.x)

