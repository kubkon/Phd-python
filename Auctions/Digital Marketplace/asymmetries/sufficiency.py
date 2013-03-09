import argparse
import csv
import functools as fts
import numpy as np
import matplotlib.pyplot as plt
import sys

def verify_sufficiency(costs, bids, b_upper, cdfs, step=100):
  n = len(lower_extremities)
  best_responses = []
  sampled_costs = [c[::step] for c in costs]
  for i in range(n):
    best_response = []
    for c in sampled_costs[i]:
      feasible_bids = np.linspace(c, b_upper, 100)
      utility = []
      for b in feasible_bids:
        if b < bids[0]:
          utility += [(b, (b-c))]
        else:
          diffs = list(map(lambda x: abs(x-b), bids))
          index = diffs.index(min(diffs))
          t_cdfs = list(filter(lambda x: cdfs.index(x) != i, cdfs))
          indexes = [j for j in range(n) if j != i]
          probability = fts.reduce(lambda x,y: x*y, [(1-cdfs[i](costs[i][index])) for i in indexes], 1)
          utility += [(b, (b-c)*probability)]
      best_response += [max(utility, key=lambda x: x[1])[0]]
    best_responses += [best_response]
  return sampled_costs, best_responses

# Uniform CDF
def F(bounds, x):
  if x < bounds[0]:
    return 0.0
  elif x > bounds[1]:
    return 1.0
  else:
    return (x - bounds[0]) / (bounds[1] - bounds[0])

### Parse command line arguments
parser = argparse.ArgumentParser(description="Numerical approximation -- sufficiency analyzer")
parser.add_argument('file_name', help='file with approximation results')
args = parser.parse_args()
file_name = args.file_name

# Read data from file
data_in = {}
with open(file_name, 'rt') as f:
  f_reader = csv.DictReader(f, delimiter=' ')
  for row in f_reader:
    for key in row:
      data_in[key] = row[key]

# Parse data common to ODE and polynomial methods
w = float(data_in['w'])
reps = [float(r.replace("[", "").replace("]", "")) for r in data_in['reps'].split(',')]
n = len(reps)
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

# Parse the rest of the data
try:
  bids = [float(b.replace("[","").replace("]","")) for b in data_in['bids'].split(',')]
  costs = []
  for i in range(n):
    label = 'costs_{}'.format(i)
    costs += [[float(c.replace("[","").replace("]","")) for c in data_in[label].split(',')]]
except KeyError:
  bs = [float(data_in['b_lower']), float(data_in['b_upper'])]
  css = []
  for i in range(n):
    label = 'cs_{}'.format(i)
    cs = [float(c.replace("[","").replace("]","")) for c in data_in[label].split(',')]
    css += [cs]

# Verify sufficiency
cdfs = [fts.partial(F, bounds) for bounds in zip(lower_extremities, upper_extremities)]
try:
  step = len(bids) // 100
  s_costs, s_bids = verify_sufficiency(costs, bids, b_upper, cdfs, step=step)
except NameError:
  # Define inverse bid function
  def cost_func(l, cs, x):
    return l + sum([c*(x-bs[0])**i for c,i in zip(cs, range(len(cs)))])
  # Compute bids and costs
  bids = np.linspace(bs[0], bs[1], 10000)
  cost_funcs = [fts.partial(cost_func, l, cs) for l,cs in zip(lower_extremities, css)]
  costs = [[f(b) for b in bids] for f in cost_funcs]
  step = len(bids) // 100
  s_costs, s_bids = verify_sufficiency(costs, bids, b_upper, cdfs, step=step)

# Plot
plt.figure()
for c in costs:
  plt.plot(c, bids)
plt.grid()
plt.xlim([0.0,1.0])
plt.ylim([0.3,1.0])
labels = ['Bidder {}'.format(i) for i in range(n)]
plt.legend(labels, loc='upper left')
plt.savefig('approximation.pdf')

plt.figure()
for c in costs:
  plt.plot(c, bids)
for c, b in zip(s_costs, s_bids):
  plt.plot(c, b, '.')
plt.grid()
plt.xlim([0.0,1.0])
plt.ylim([0.3,1.0])
labels = ['Bidder {}'.format(i) for i in range(n)]
labels += ['BR {}'.format(i) for i in range(n)]
plt.legend(labels, loc='upper left')
plt.savefig('sufficiency.pdf')
