import functools as fts
import matplotlib.pyplot as plt
import numpy as np

def approximate_bids(w, reps):
  def F(bounds, x):
    if x < bounds[0]:
      return 0.0
    elif x > bounds[1]:
      return 1.0
    else:
      return (x - bounds[0]) / (bounds[1] - bounds[0])

  def runge_kutta(odes, initials, step, end):
    tables = [[init] for init in initials]
    x = initials[0][0]
    n = len(odes)
    yss = {i: y for (i, y) in zip(range(n), map(lambda init: init[1], initials))}
    while x < end:
      vs1 = []
      for i in range(n):
        y = yss[i]
        ys = [yss[j] for j in range(n) if j != i]
        vs1 += [step*odes[i](x, y, ys)]
      vs2 = []
      for i in range(n):
        y = yss[i] + 0.5*vs1[i]
        ys = [yss[j] + 0.5*vs1[j] for j in range(n) if j != i]
        vs2 += [step*odes[i](x + 0.5*step, y, ys)]
      vs3 = []
      for i in range(n):
        y = yss[i] + 0.5*vs2[i]
        ys = [yss[j] + 0.5*vs2[j] for j in range(n) if j != i]
        vs3 += [step*odes[i](x + 0.5*step, y, ys)]
      vs4 = []
      for i in range(n):
        y = yss[i] + vs3[i]
        ys = [yss[j] + vs3[j] for j in range(n) if j != i]
        vs4 += [step*odes[i](x + step, y, ys)]
      x += step
      for i in range(n):
        yss[i] += 1/6 * (vs1[i] + 2*vs2[i] + 2*vs3[i] + vs4[i])
        if x < end:
          tables[i] += [(x, yss[i])]
    return tables

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
  ode = lambda d, x, y, ys: (d - y) / (n - 1) * ((2 - n) / (x - y) + sum(map(lambda t: 1 / (x - t), ys)))
  odes = [fts.partial(ode, d) for d in upper_extremities]
  # Numerical approximation (Runga-Kutta)
  step = 0.00001
  error = 0.000001
  high = b_upper
  low = lower_extremities[0]
  guess_bid = 0.5*(low + high)
  guess_costs = [lower_extremities[-1] for i in range(n)]
  while (high - low) >= error:
    tables = runge_kutta(odes, [(guess_bid, cost) for cost in guess_costs], step, b_upper)
    bids = [list(map(lambda x: x[0], table)) for table in tables]
    costs = [list(map(lambda x: x[1], table)) for table in tables]
    condition1 = [all(map(lambda x: x >= lower_extremities[0] and x <= b_upper, cost)) for cost in costs]
    condition2 = [all(x < y for x, y in zip(cost, bid)) for cost, bid in zip(costs, bids)]
    if all(condition1) and all(condition2):
      high = guess_bid
    else:
      low = guess_bid
    # Update guessed initial bid
    guess_bid = 0.5*(low + high)
    cost = max(guess_costs)
    # Update guessed initial costs
    for i in range(1, n):
      if guess_bid > lower_extremities[i]:
        tmp = guess_bid - i / (sum([1/(guess_bid - l) for l in lower_extremities[:i+1]]))
        print("k={}: {}".format(i+1, tmp))
        if lower_extremities[i] <= tmp:
          try:
            if tmp < lower_extremities[i+1]:
              cost = tmp
              break
          except IndexError:
            cost = tmp
    guess_costs = [min(l, cost) for l in lower_extremities]
  print("Initial bid: ", guess_bid)
  print("Initial costs: ", guess_costs)
  return tables

# Scenario
w = 0.6
reps = [0.25, 0.5, 0.5]
tables = approximate_bids(w, reps)
for table in tables:
  print(table[-1][1], table[-1][0])
bids = [list(map(lambda x: x[0], table)) for table in tables]
costs = [list(map(lambda x: x[1], table)) for table in tables]
plt.figure()
for c, b in zip(costs, bids):
  plt.plot(c, b)
plt.grid()
plt.legend(['Bidder {}'.format(i) for i in range(len(reps))], loc='upper left')
plt.savefig('bids.pdf')

