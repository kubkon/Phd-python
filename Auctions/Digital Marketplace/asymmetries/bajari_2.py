import matplotlib.pyplot as plt
import numpy as np

def runge_kutta(odes, initials, step, end):
  table_y1, table_y2, table_y3 = [initials[0]], [initials[1]], [initials[2]]
  x = initials[0][0]
  y1 = initials[0][1]
  y2 = initials[1][1]
  y3 = initials[2][1]
  while x < end:
    v1 = step * odes[0](x, y1, y2, y3)
    w1 = step * odes[1](x, y1, y2, y3)
    u1 = step * odes[2](x, y1, y2, y3)

    v2 = step * odes[0](x + 0.5*step, y1 + 0.5*v1, y2 + 0.5*w1, y3 + 0.5*u1)
    w2 = step * odes[1](x + 0.5*step, y1 + 0.5*v1, y2 + 0.5*w1, y3 + 0.5*u1)
    u2 = step * odes[2](x + 0.5*step, y1 + 0.5*v1, y2 + 0.5*w1, y3 + 0.5*u1)

    v3 = step * odes[0](x + 0.5*step, y1 + 0.5*v2, y2 + 0.5*w2, y3 + 0.5*u2)
    w3 = step * odes[1](x + 0.5*step, y1 + 0.5*v2, y2 + 0.5*w2, y3 + 0.5*u2)
    u3 = step * odes[2](x + 0.5*step, y1 + 0.5*v2, y2 + 0.5*w2, y3 + 0.5*u2)

    v4 = step * odes[0](x + step, y1 + v3, y2 + w3, y3 + u3)
    w4 = step * odes[1](x + step, y1 + v3, y2 + w3, y3 + u3)
    u4 = step * odes[2](x + step, y1 + v3, y2 + w3, y3 + u3)

    y1 += 1/6 * (v1 + 2*v2 + 2*v3 + v4)
    y2 += 1/6 * (w1 + 2*w2 + 2*w3 + w4)
    y3 += 1/6 * (u1 + 2*u2 + 2*u3 + u4)
    x += step
    if x < end:
      table_y1 += [(x, y1)]
      table_y2 += [(x, y2)]
      table_y3 += [(x, y3)]
  return (table_y1, table_y2, table_y3)

# Scenario
w = 0.75
r1 = 0.25
r2 = 0.5
r3 = 0.75
c1 = [(1-w)*r1, (1-w)*r1 + w]
c2 = [(1-w)*r2, (1-w)*r2 + w]
c3 = [(1-w)*r3, (1-w)*r3 + w]
b_upper = ((c1[1] + c2[1] + c3[1]) + np.sqrt((c1[1]+c2[1]+c3[1])**2 - 3*(c1[1]*c2[1] + c1[1]*c3[1] + c2[1]*c3[1]))) / 3
b_upper = 0.839
print("Terminal condition: ", b_upper)

# Numerical approximation (Runge-Kutta)
ode1 = lambda x, y1, y2, y3: 0.5*(c1[1]-y1)*(1/(x-y2) + 1/(x-y3) - 1/(x-y1))
ode2 = lambda x, y1, y2, y3: 0.5*(c2[1]-y2)*(1/(x-y1) + 1/(x-y3) - 1/(x-y2))
ode3 = lambda x, y1, y2, y3: 0.5*(c3[1]-y3)*(1/(x-y1) + 1/(x-y2) - 1/(x-y3))
step = 0.00001
error = 0.000001
high = b_upper
low = c1[0]
guess_bid = 0.5*(low + high)
guess_costs = [c1[0], c1[0], c1[0]]
while (high - low) >= error:
  rk_y1, rk_y2, rk_y3 = runge_kutta((ode1, ode2, ode3), ((guess_bid, guess_costs[0]), (guess_bid, guess_costs[1]), (guess_bid, guess_costs[2])), 
      step, b_upper)
  bids_y1 = list(map(lambda x: x[0], rk_y1))
  args_y1 = list(map(lambda x: x[1], rk_y1))
  bids_y2 = list(map(lambda x: x[0], rk_y2))
  args_y2 = list(map(lambda x: x[1], rk_y2))
  bids_y3 = list(map(lambda x: x[0], rk_y3))
  args_y3 = list(map(lambda x: x[1], rk_y3))
  if (all(map(lambda x: x>=c1[0] and x<=b_upper, args_y1)) and
      all(map(lambda x: x>=c1[0] and x<=b_upper, args_y2)) and
      all(map(lambda x: x>=c1[0] and x<=b_upper, args_y3)) and
      all(x<y for x,y in zip(args_y1, bids_y1)) and
      all(x<y for x,y in zip(args_y2, bids_y2)) and
      all(x<y for x,y in zip(args_y3, bids_y3))):
    high = guess_bid
  else:
    low = guess_bid
  # Update guessed initial bid
  guess_bid = 0.5*(low + high)
  # Update guesses initial costs
  cost_3 = guess_bid - 2 / (sum([1/(guess_bid - c1[0]),1/(guess_bid - c2[0]),1/(guess_bid - c3[0])]))
  cost_2 = guess_bid - 1 / (sum([1/(guess_bid - c1[0]),1/(guess_bid - c2[0])]))
  if cost_2>=c2[0] and cost_2<c3[0]:
    guess_costs = [min(c1[0], cost_2), min(c2[0], cost_2), min(c3[0], cost_2)]
  elif cost_3>=c3[0]:
    guess_costs = [min(c1[0], cost_3), min(c2[0], cost_3), min(c3[0], cost_3)]
  else:
    pass
print("Guessed initial bid: ", guess_bid)
print("Guessed initial costs: ", guess_costs)

# Plot
plt.figure()
plt.plot(list(map(lambda x: x[1], rk_y1)), list(map(lambda x: x[0], rk_y1)))
plt.plot(list(map(lambda x: x[1], rk_y2)), list(map(lambda x: x[0], rk_y2)), '.-')
plt.plot(list(map(lambda x: x[1], rk_y3)), list(map(lambda x: x[0], rk_y3)), '--')
plt.legend(['Bidder 1', 'Bidder 2', 'Bidder 3'], loc="upper left")
plt.grid()
plt.savefig('bids.pdf')

