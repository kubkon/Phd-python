import functools as fts
import matplotlib.pyplot as plt
import numpy as np
import scipy.integrate as scint

# Scenario
w = 0.75
reps = [0.25, 0.5, 0.95]
# Infer number of bidders
n = len(reps)
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
if n == 2:
  objective_func = lambda x: (x - upper_extremities[0])*(1 - cdfs[0](x))
else:
  objective_func = lambda x: (x - upper_extremities[0])*fts.reduce(lambda p, r: p*r, [(1-cdf(x)) for cdf in cdfs])
tabulated = [objective_func(b) for b in bs]
maximum = max(tabulated)
b_upper = min([b for b, i in zip(bs, range(len(bs))) if tabulated[i] == maximum])
print("Terminal condition: ", b_upper)

# Numerical approximation
# Define function describing the system of ODEs
def ode(k, ys, t):
  M = 1/(k-1) * (np.diag([1-k for i in range(k)]) + np.ones((k,k)))
  I = np.array([1/(t-y) for y in ys])
  return [(upper_extremities[i]-ys[i]) / (k-1) * (M.dot(I))[i] for i in range(k)]
# Define function describing bidding extension (below cost)
def extension(k, ys, t):
  return t - (k - 1) / sum([1/(t-y) for y in ys])
# Initialize the algorithm
guess_bid = 0.317840938026
results = {}
for b_lower in np.linspace(guess_bid-0.05, guess_bid+0.05, 1000):  
  # Update initial costs
  cost = None
  for i in range(1, n):
    if b_lower > lower_extremities[i]:
      tmp = b_lower - i / (sum([1/(b_lower - l) for l in lower_extremities[:i+1]]))
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
  bids = np.linspace(b_lower, b_upper, 10000)
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
          bids = np.linspace(bids[stop_index], b_upper, len(bids)-stop_index)
          k += lower_extremities.count(lower_extremities[k]) - 1
        else:
          tmps = [list(map(lambda x: x[i], ode_tables)) for i in range(k)]
        for i in range(n):
          costs[i] += tmps[i][1:]
        return recur(k+1, costs, bids)
    costs, _ = recur(k, [[c] for c in init_costs], bids)
  condition1 = [all(map(lambda x: x >= i_cost and x <= b_upper, cost)) for i_cost, cost in zip(init_costs, costs)]
  condition2 = [all(x < y for x, y in zip(cost[:-1], bids[:-1])) for cost in costs]
  if all(condition1) and all(condition2):
    results[b_lower] = costs

# Select result for which distance between the latest cost
# and terminal value is the smallest
refined = [min([(b, abs(b_upper - cs[i][-1])) for b, cs in results.items()], key=lambda x: x[1]) for i in range(n)]
print("Refined: ", refined)

if n == 2:
  # Estimate analytical solution
  c1 = [(1-w)*reps[0], (1-w)*reps[0] + w]
  c2 = [(1-w)*reps[1], (1-w)*reps[1] + w]
  b = [(c1[0]*c2[0] - ((c1[1] + c2[1]) / 2)**2) / (c1[0] - c1[1] + c2[0] - c2[1]),
       (c1[1] + c2[1]) / 2]
  # Constants of integration
  d1 = ((c2[1]-c1[1])**2 + 4*(b[0]-c2[1])*(c1[0]-c1[1])) / (-2*(b[0]-b[1])*(c1[0]-c1[1])) * np.exp((c2[1]-c1[1]) / (2*(b[0]-b[1])))
  d2 = ((c1[1]-c2[1])**2 + 4*(b[0]-c1[1])*(c2[0]-c2[1])) / (-2*(b[0]-b[1])*(c2[0]-c2[1])) * np.exp((c1[1]-c2[1]) / (2*(b[0]-b[1])))
  # Inverse bid functions
  inv1 = lambda x: c1[1] + (c2[1]-c1[1])**2 / (d1*(c2[1]+c1[1]-2*x)*np.exp((c2[1]-c1[1])/(c2[1]+c1[1]-2*x)) + 4*(c2[1]-x))
  inv2 = lambda x: c2[1] + (c1[1]-c2[1])**2 / (d2*(c1[1]+c2[1]-2*x)*np.exp((c1[1]-c2[1])/(c1[1]+c2[1]-2*x)) + 4*(c1[1]-x))
  # Plot
  bs = np.linspace(b[0], b[1], 50)
  theory = [(list(map(inv1, bs)), bs), (list(map(inv2, bs)), bs)]

# Plot all results
plt.figure()
for b in results:
  for i in range(n):
    plt.subplot(221 + i)
    plt.plot(results[b][i], bids)
    plt.grid()
    plt.title("Bidder {}".format(i))

if n == 2:
  plt.subplot(211)
  plt.plot(theory[0][0], theory[0][1], '.')
  plt.subplot(212)
  plt.plot(theory[1][0], theory[1][1], '.')

plt.savefig('lebrun_refine_all.pdf')

# Plot refined average
plt.figure()
for c in results[refined[0][0]]:
  plt.plot(c, bids)

if n == 2:
  plt.plot(theory[0][0], theory[0][1], '.')
  plt.plot(theory[1][0], theory[1][1], '.')

plt.grid()
plt.legend(['Bidder {}'.format(i) for i in range(n)], loc='upper left')
plt.savefig('lebrun_refine.pdf')

