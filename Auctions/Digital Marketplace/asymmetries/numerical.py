import matplotlib.pyplot as plt
import numpy as np

def euler(odes, initials, step, end):
  table_y1, table_y2 = [initials[0]], [initials[1]]
  x = initials[0][0]
  y1 = initials[0][1]
  y2 = initials[1][1]
  while x < end:
    y1 += step * odes[0](x,y1,y2)
    y2 += step * odes[1](x,y1,y2)
    x += step
    if x < end:
      table_y1 += [(x, y1)]
      table_y2 += [(x, y2)]
  return (table_y1, table_y2)

def runge_kutta(odes, initials, step, end):
  table_y1, table_y2 = [initials[0]], [initials[1]]
  x = initials[0][0]
  y1 = initials[0][1]
  y2 = initials[1][1]
  while x < end:
    v1 = step * odes[0](x, y1, y2)
    w1 = step * odes[1](x, y1, y2)
    v2 = step * odes[0](x + 0.5*step, y1 + 0.5*v1, y2 + 0.5*w1)
    w2 = step * odes[1](x + 0.5*step, y1 + 0.5*v1, y2 + 0.5*w1)
    v3 = step * odes[0](x + 0.5*step, y1 + 0.5*v2, y2 + 0.5*w2)
    w3 = step * odes[1](x + 0.5*step, y1 + 0.5*v2, y2 + 0.5*w2)
    v4 = step * odes[0](x + step, y1 + v3, y2 + w3)
    w4 = step * odes[1](x + step, y1 + v3, y2 + w3)
    y1 += 1/6 * (v1 + 2*v2 + 2*v3 + v4)
    y2 += 1/6 * (w1 + 2*w2 + 2*w3 + w4)
    x += step
    if x < end:
      table_y1 += [(x, y1)]
      table_y2 += [(x, y2)]
  return (table_y1, table_y2)

# Scenario
w = 0.5
r1 = 0.5
r2 = 0.4
c1 = [(1-w)*r1, (1-w)*r1 + w]
c2 = [(1-w)*r2, (1-w)*r2 + w]
b = [(c1[0]*c2[0] - ((c1[1] + c2[1]) / 2)**2) / (c1[0] - c1[1] + c2[0] - c2[1]),
    (c1[1] + c2[1]) / 2]

# Numerical approximation (Euler & Runge-Kutta)
ode1 = lambda x, y1, y2: (y1 - c1[1]) / (y2 - x)
ode2 = lambda x, y1, y2: (y2 - c2[1]) / (y1 - x)
step = 0.01
e_y1, e_y2 = euler((ode1, ode2), ((b[0], c1[0]), (b[0], c2[0])),
    step, b[1])
rk_y1, rk_y2 = runge_kutta((ode1, ode2), ((b[0], c1[0]), (b[0], c2[0])),
    step, b[1])

# Explicitly derived
# Constants of integration
d1 = ((c2[1]-c1[1])**2 + 4*(b[0]-c2[1])*(c1[0]-c1[1])) / (-2*(b[0]-b[1])*(c1[0]-c1[1])) * np.exp((c2[1]-c1[1]) / (2*(b[0]-b[1])))
d2 = ((c1[1]-c2[1])**2 + 4*(b[0]-c1[1])*(c2[0]-c2[1])) / (-2*(b[0]-b[1])*(c2[0]-c2[1])) * np.exp((c1[1]-c2[1]) / (2*(b[0]-b[1])))
# Inverse bid functions
inv1 = lambda x: c1[1] + (c2[1]-c1[1])**2 / (d1*(c2[1]+c1[1]-2*x)*np.exp((c2[1]-c1[1])/(c2[1]+c1[1]-2*x)) + 4*(c2[1]-x))
inv2 = lambda x: c2[1] + (c1[1]-c2[1])**2 / (d2*(c1[1]+c2[1]-2*x)*np.exp((c1[1]-c2[1])/(c1[1]+c2[1]-2*x)) + 4*(c1[1]-x))

# Plot
plt.figure()
ys = list(map(lambda x: x[0], rk_y1))
#plt.plot(list(map(lambda x: x[1], e_y1)), ys, '*')
plt.plot(list(map(lambda x: x[1], rk_y1)), ys, 'o')
plt.plot(list(map(inv1, np.linspace(b[0], b[1], 100))), np.linspace(b[0], b[1], 100))
plt.grid()
plt.savefig('bidder_1.pdf')
plt.figure()
#plt.plot(list(map(lambda x: x[1], e_y2)), ys, '*')
plt.plot(list(map(lambda x: x[1], rk_y2)), ys, 'o')
plt.plot(list(map(inv2, np.linspace(b[0], b[1], 100))), np.linspace(b[0], b[1], 100))
plt.grid()
plt.savefig('bidder_2.pdf')

