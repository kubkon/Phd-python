import functools as fts
import matplotlib.pyplot as plt
import numpy as np
import scipy.integrate as scint

def approximate_bids(w, reps):
  def f(bounds, x):
    if x < bounds[0]:
      return 0.0
    elif x > bounds[1]:
      return 0.0
    else:
      return 1 / (bounds[1] - bounds[0])
  
  def F(bounds, x):
    if x < bounds[0]:
      return 0.0
    elif x > bounds[1]:
      return 1.0
    else:
      return (x - bounds[0]) / (bounds[1] - bounds[0])

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
  # Create system of ODEs
  def ode(ys, t):
    M = 1/(n-1) * (np.diag([1-n for i in range(n)]) + np.ones((n,n)))
    I = np.array([1/(t-y) for y in ys])
    return [(upper_extremities[i]-ys[i]) / (n-1) * (M.dot(I))[i] for i in range(n)]
  # Numerical approximation
  # Initialize
  error = 0.000001
  high = b_upper
  low = lower_extremities[1]
  while (high - low) >= error:
    # Update guessed value of initial bid
    guess_bid = 0.5*(low + high)
    # Update guessed initial costs
    for i in range(1, n):
      if guess_bid > lower_extremities[i]:
        tmp = guess_bid + i / (sum([1/(guess_bid - l) for l in lower_extremities[:i+1]]))
        if lower_extremities[i] <= tmp:
          try:
            if tmp < lower_extremities[i+1]:
              cost = tmp
              k = i+1
              print("{}: {}".format(k, cost))
              break
          except IndexError:
            cost = tmp
            k = i+1
            print("{}: {}".format(k, cost))
    guess_costs = []
    for l in lower_extremities:
      try:
        guess_costs += [min(l, cost)]
      except NameError:
        guess_costs += [l]
    print(guess_costs)
    bids = np.linspace(guess_bid, b_upper, 100000)
    tables = scint.odeint(ode, guess_costs, bids)
    costs = [list(map(lambda x: x[i], tables)) for i in range(n)]
    condition1 = [all(map(lambda x: x >= g_cost and x <= b_upper, cost)) for g_cost, cost in zip(guess_costs, costs)]
    #condition1 = [all(map(lambda x: x >= lower_extremities[0] and x <= b_upper, cost)) for cost in costs]
    condition2 = [all(x < y for x, y in zip(cost, bids)) for cost in costs]
    if all(condition1) and all(condition2):
      high = guess_bid
    else:
      low = guess_bid
  print("Initial bid: ", guess_bid)
  print("Initial costs: ", guess_costs)
  return (bids, costs)

# Scenario
w = 0.5
reps = [0.6, 0.65, 0.7]
bids, costs = approximate_bids(w, reps)
for cost in costs:
  print(bids[-1], cost[-1])
plt.figure()
for cost in costs:
  plt.plot(cost, bids)
plt.grid()
plt.legend(['Bidder {}'.format(i) for i in range(len(reps))], loc='upper left')
plt.savefig('bids.pdf')

