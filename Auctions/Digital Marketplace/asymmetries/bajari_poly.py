from lmfit import minimize, Parameters, Parameter
import functools as fts
import numpy as np
import scipy.optimize as scopt

# Scenario
w = 0.75
reps = [0.25, 0.75]
# Estimate cost support bounds
lower_extremities = [(1-w)*r for r in reps]
upper_extremities = [(1-w)*r+w for r in reps]

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
objective_func = lambda x: (x - upper_extremities[0])*fts.reduce(lambda p, r: p*r, [(1-cdf(x)) for cdf in cdfs], 1)
tabulated = [objective_func(b) for b in bs]
maximum = max(tabulated)
b_upper = min([b for b, i in zip(bs, range(len(bs))) if tabulated[i] == maximum])

# Approximate bids using non-linear least squares
params = Parameters()
params.add('b_lower', value=lower_extremities[1], min=lower_extremities[1], max=b_upper)
params.add_many(('a_0', 1, True, None, None, None),
                ('a_1', 1, True, None, None, None),
                ('a_2', 1, True, None, None, None),
                ('a_3', 1, True, None, None, None),
                ('a_4', 1, True, None, None, None),
                ('a_5', 1, True, None, None, None),
                ('b_0', 1, True, None, None, None),
                ('b_1', 1, True, None, None, None),
                ('b_2', 1, True, None, None, None),
                ('b_3', 1, True, None, None, None),
                ('b_4', 1, True, None, None, None),
                ('b_5', 1, True, None, None, None))

def func2min(params, K=5, ngrid=1000):
  # Unpack params
  b_lower = params['b_lower'].value
  ps = ([params['a_{}'.format(i)].value for i in range(K+1)],
        [params['b_{}'.format(i)].value for i in range(K+1)])
  n = 2
  # Define helper functions
  def grid(b_lower, t):
    return b_lower + t*(b_upper - b_lower)/(ngrid + 1)
  def cost_func(b_lower, ps, t):
    B = np.array([(t - b_lower)**k for k in range(K+1)])
    return b_lower + np.dot(ps, B)
  def d_cost_func(b_lower, ps, t):
    bs = [0] + [(t - b_lower)**(k-1) for k in range(1, K+1)]
    B = np.array(bs)
    return np.dot(ps, B)
  def g(i, b_lower, ps, t):
    indexes = [j for j in range(n) if j!=i]
    p_sum = sum([d_cost_func(b_lower,ps[j],t)/(upper_extremities[j]-cost_func(b_lower,ps[j],t)) for j in indexes])
    return 1 + p_sum*(t-cost_func(b_lower, ps[i], t))

  out = []
  for i in range(n):
    out += [g(i, b_lower, ps, grid(b_lower, t))**2 for t in range(ngrid + 1)]
    out += [(lower_extremities[i] - cost_func(b_lower, ps[i], b_lower))**2,
            (b_upper - cost_func(b_lower, ps[i], b_upper))**2]
  #p_sum1 = [g(i, b_lower, ps, grid(b_lower, t))**2 for i in range(n) for t in range(ngrid+1)]
  #p_sum2 = [(lower_extremities[i] - cost_func(b_lower, ps[i], b_lower))**2 for i in range(n)]
  #p_sum3 = [(b_upper - cost_func(b_lower, ps[i], b_upper))**2 for i in range(n)]
  return sum(out)

result = minimize(func2min, params, kws={'K': 5, 'ngrid': 1000}, method='bfgs')
print(params)

