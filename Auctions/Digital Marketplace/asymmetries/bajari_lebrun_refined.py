import functools as fts
import matplotlib.pyplot as plt
import numpy as np
import scipy.integrate as scint

def forward_shooting(bs, lower_extremities, upper_extremities, granularity=10000):
  # Define function describing the system of ODEs
  def ode(k, ys, t):
    M = 1/(k-1) * (np.diag([1-k for i in range(k)]) + np.ones((k,k)))
    I = np.array([1/(t-y) for y in ys])
    return [(upper_extremities[i]-ys[i]) / (k-1) * (M.dot(I))[i] for i in range(k)]
  # Define function describing bidding extension (below cost)
  def extension(k, ys, t):
    return t - (k - 1) / sum([1/(t-y) for y in ys])
  
  # Infer number of bidders
  n = len(lower_extremities)
  # Update initial costs
  cost = None
  for i in range(1, n):
    if bs[0] > lower_extremities[i]:
      tmp = bs[0] - i / (sum([1/(bs[0] - l) for l in lower_extremities[:i+1]]))
      if lower_extremities[i] <= tmp:
        try:
          if tmp < lower_extremities[i+1]:
            cost = tmp
            k = i+1
            break
        except IndexError:
          cost = tmp
          k = i+1
  init_costs = [min(l, cost) if cost is not None else l for l in lower_extremities]
  bids = np.linspace(bs[0], bs[1], granularity)
  if k >= n:
    node = lambda ys, t: ode(n, ys, t)
    tables = scint.odeint(node, init_costs, bids)
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
          bids = np.linspace(bids[stop_index], bs[1], len(bids)-stop_index)
          k += lower_extremities.count(lower_extremities[k]) - 1
        else:
          tmps = [list(map(lambda x: x[i], ode_tables)) for i in range(k)]
        for i in range(n):
          costs[i] += tmps[i][1:]
        return recur(k+1, costs, bids)
    costs, _ = recur(k, [[c] for c in init_costs], bids)
  return costs, bids

def initial_estimate(b_upper, lower_extremities, upper_extremities, error=0.00001):
  # Initialize the algorithm
  high = b_upper
  low = lower_extremities[1]
  while not (high - low < error):
    # Update guessed value of the initial bid
    guess_bid = 0.5*(low + high)
    # Perform forward shooting
    costs, bids = forward_shooting([guess_bid, b_upper], lower_extremities, upper_extremities)
    condition1 = [all(map(lambda x: x >= cost[0] and x <= b_upper, cost)) for cost in costs]
    condition2 = [all(x < y for x, y in zip(cost[:-1], bids[:-1])) for cost in costs]
    if all(condition1) and all(condition2):
      high = guess_bid
    else:
      low = guess_bid
  return guess_bid

def refined_estimate(bs, lower_extremities, upper_extremities, granularity=1000):
  results = {}
  for b in np.linspace(bs[0]-0.05, bs[0]+0.05, granularity):
    costs, bids = forward_shooting([b, bs[1]], lower_extremities, upper_extremities)
    condition1 = [all(map(lambda x: x >= cost[0] and x <= bs[1], cost)) for cost in costs]
    condition2 = [all(x < y for x, y in zip(cost[:-1], bids[:-1])) for cost in costs]
    if all(condition1) and all(condition2):
      results[b] = costs
  # Select result for which distance between the latest cost
  # and terminal value is the smallest
  n = len(lower_extremities)
  refined = [min([(b, abs(bs[1] - cs[i][-1])) for b, cs in results.items()], key=lambda x: x[1]) for i in range(n)]
  # FIX:ME
  return refined[0][0]

# Scenario
w = 0.75
reps = [0.25, 0.5, 0.95]
# Estimate cost support bounds
lower_extremities = [(1-w)*r for r in reps]
upper_extremities = [(1-w)*r + w for r in reps]

# Estimate upper bound on bids
# Define uniform CDF (for convenience)
def F(bounds, x):
  if x < bounds[0]:
    return 0.0
  elif x > bounds[1]:
    return 1.0
  else:
    return (x - bounds[0]) / (bounds[1] - bounds[0])
# Find maximum
bs = np.linspace(upper_extremities[0], upper_extremities[1], 10000)
cdfs = list(map(lambda ext: fts.partial(F, ext), zip(lower_extremities[1:], upper_extremities[1:])))
if len(reps) == 2:
  objective_func = lambda x: (x - upper_extremities[0])*(1 - cdfs[0](x))
else:
  objective_func = lambda x: (x - upper_extremities[0])*fts.reduce(lambda p, r: p*r, [(1-cdf(x)) for cdf in cdfs])
tabulated = [objective_func(b) for b in bs]
maximum = max(tabulated)
b_upper = min([b for b, i in zip(bs, range(len(bs))) if tabulated[i] == maximum])

# Initial estimate of the lower bound on bids
b_lower = initial_estimate(b_upper, lower_extremities, upper_extremities)
# Refined estimate of the lower bound on bids
b_lower = refined_estimate([b_lower, b_upper], lower_extremities, upper_extremities)
# Approximate using refined estimate
costs, bids = forward_shooting([b_lower, b_upper], lower_extremities, upper_extremities)

print("Upper boundary value: ", b_upper)
print("Lower boundary value: ", b_lower)

# Plot
plt.figure()
for c in costs:
  plt.plot(c, bids)
plt.grid()
plt.legend(['Bidder {}'.format(i) for i in range(len(reps))], loc='upper left')
plt.savefig('approximation.pdf')

