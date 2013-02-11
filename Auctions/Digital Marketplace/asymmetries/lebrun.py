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
reps = [0.25, 0.75]
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
  bids = np.linspace(guess_bid, b_upper-0.1, 10000)
  if k >= n:
    node = lambda ys, t: ode(n, ys, t)
    tables = scint.odeint(node, guess_costs, bids)
    costs = [list(map(lambda x: x[i], tables)) for i in range(k)]
  else:
    def recur(k, costs, bids):
      if k > n:
        return costs, bids
      else:
        kode = lambda ys, t: ode(k, ys, t)
        ode_tables = scint.odeint(kode, [costs[i][-1] for i in range(k)], bids)
        if k < n:
          ext_tables = [extension(k, t, b) for t, b in zip(ode_tables, bids)]
          diffs = list(map(lambda x: abs(x - lower_extremities[k]), ext_tables))
          stop_index = diffs.index(min(diffs))
          tmps = [list(map(lambda x: x[i], ode_tables[:stop_index+1])) for i in range(k)]
          tmps += [ext_tables[:stop_index+1] for i in range(n-k)]
          bids = np.linspace(bids[stop_index], b_upper, len(bids)-stop_index)
          k += lower_extremities.count(lower_extremities[k]) - 1
        else:
          tmps = [list(map(lambda x: x[i], ode_tables)) for i in range(k)]
        for i in range(n):
          costs[i] += tmps[i][1:]
        return recur(k+1, costs, bids)
    costs, _ = recur(k, [[c] for c in guess_costs], bids)
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
plt.legend(['Bidder {}'.format(i) for i in range(n)], loc='upper left')
plt.savefig('lebrun.pdf')

