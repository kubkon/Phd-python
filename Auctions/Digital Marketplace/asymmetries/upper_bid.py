import numpy as np
import scipy.optimize as opt
import matplotlib.pyplot as plt

w = 0.5
reps = [0.25, 0.5, 0.75]
c1 = [(1-w)*reps[0], (1-w)*reps[0]+w]
c2 = [(1-w)*reps[1], (1-w)*reps[1]+w]
c3 = [(1-w)*reps[2], (1-w)*reps[2]+w]

def F2(x):
  if x < c2[0]:
    return 0.0
  elif x > c2[1]:
    return 1.0
  else:
    return (x - c2[0]) / (c2[1] - c2[0])

def F3(x):
  if x < c3[0]:
    return 0.0
  elif x > c3[1]:
    return 1.0
  else:
    return (x - c3[0]) / (c3[1] - c3[0])

def objective_func(x):
  return (c1[1] - x)*(1 - F2(x))*(1 - F3(x))

bounds = [(c1[1], c2[1])]

bound1 = lambda x: x-c1[1]
bound2 = lambda x: c2[1]-x

result = opt.fmin_slsqp(objective_func, np.array([0.0]), ieqcons=[bound1, bound2])
print("Optimization result: ", result)

b = np.linspace(c1[1], c2[1], 10000)
plt.figure()
plt.plot(b, [(-1)*objective_func(x) for x in b])
plt.grid()
plt.savefig('hmm.pdf')

