import functools as fts
import matplotlib.pyplot as plt
import numpy as np
import scipy.integrate as scint

def F(bounds, x):
  if x < bounds[0]:
    return 0.0
  elif x > bounds[1]:
    return 1.0
  else:
    return (x - bounds[0]) / (bounds[1] - bounds[0])

# Scenario
w = 0.5
reps = [0.25, 0.5, 0.75]
n = len(reps)
lower_extremities = [(1-w)*r for r in reps]
upper_extremities = [(1-w)*r + w for r in reps]

# Estimate upper bound on bids
bs = np.linspace(upper_extremities[0], upper_extremities[1], 10000)
cdfs = list(map(lambda ext: fts.partial(F, ext), zip(lower_extremities[1:], upper_extremities[1:])))
if n == 2:
  objective_func = lambda x: (x - upper_extremities[0])*(1 - cdfs[0](x))
else:
  objective_func = lambda x: (x - upper_extremities[0])*fts.reduce(lambda p, r: p*r, [(1-cdf(x)) for cdf in cdfs])
tabulated = [objective_func(b) for b in bs]
maximum = max(tabulated)
b_upper = min([b for b, i in zip(bs, range(len(bs))) if tabulated[i] == maximum])
print("Terminal condition: ", b_upper)

# Numerical approximation
# Function describing the system of ODEs
def ode(k, ys, t):
  M = 1/(k-1) * (np.diag([1-k for i in range(k)]) + np.ones((k,k)))
  I = np.array([1/(t-y) for y in ys])
  return [(upper_extremities[i]-ys[i]) / (k-1) * (M.dot(I))[i] for i in range(k)]
# Function describing bidding extension (below cost)
def extension(k, ys, t):
  return t - (k - 1) / sum([1/(t-y) for y in ys])
# Initialize
error = 0.000001
high = b_upper
low = lower_extremities[1]
while not (high - low < error):
  # Update guessed value of the initial bid
  guess_bid = 0.5*(low + high)
  # Update guessed initial costs
  cost = None
  for i in range(1, n):
    if guess_bid > lower_extremities[i]:
      tmp = guess_bid - i / (sum([1/(guess_bid - l) for l in lower_extremities[:i+1]]))
      if lower_extremities[i] <= tmp:
        try:
          if tmp < lower_extremities[i+1]:
            cost = tmp
            k = i+1
            break
        except IndexError:
          cost = tmp
          k = i+1
  guess_costs = [min(l, cost) if cost is not None else l for l in lower_extremities]
  print("k={}, costs: {}".format(k, guess_costs))
  if k >= n:
    node = lambda ys, t: ode(n, ys, t)
    bids = np.linspace(guess_bid, b_upper, 10000)
    tables = scint.odeint(node, guess_costs, bids)
    costs = [list(map(lambda x: x[i], tables)) for i in range(k)]
  else:
    # 1. for all b in [b_lower, lower_extremities[2]]
    kode = lambda ys, t: ode(k, ys, t)
    bids = np.linspace(guess_bid, b_upper, 10000)
    tables = scint.odeint(kode, guess_costs[:k], bids)
    tmps = [extension(k, t, b) for t, b in zip(tables, bids)]
    # here, need to find the value of b for which extension() gives lower extremity
    diffs = list(map(lambda x: abs(x - lower_extremities[k]), tmps))
    stop_index = diffs.index(min(diffs))
    costs = [list(map(lambda x: x[i], tables[:stop_index+1])) for i in range(k)]
    costs += [tmps[:stop_index+1]]
    # 2. for all b in (lower_extremity[2], b_upper]
    kode = lambda ys, t: ode(k+1, ys, t)
    bids_2 = np.linspace(bids[stop_index], b_upper, 10000-(stop_index+1))
    tables = scint.odeint(kode, [c[-1] for c in costs], bids_2)
    tmps = [list(map(lambda x: x[i], tables)) for i in range(k+1)]
    for i in range(k+1):
      costs[i] += tmps[i]
  condition1 = [all(map(lambda x: x >= g_cost and x <= b_upper, cost)) for g_cost, cost in zip(guess_costs, costs)]
  condition2 = [all(x < y for x, y in zip(cost[:-1], bids[:-1])) for cost in costs]
  if all(condition1) and all(condition2):
    high = guess_bid
  else:
    low = guess_bid
print("Initial bid: ", guess_bid)

# Plot
plt.figure()
for c in costs:
  plt.plot(c, bids)
plt.grid()
plt.savefig('lebrun.pdf')

