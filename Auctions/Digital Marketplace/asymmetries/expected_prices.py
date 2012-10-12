#!/usr/bin/env python2.7
# encoding: utf-8
"""
asymmetries.py

Created by Jakub Konka on 2012-05-20.
Copyright (c) 2012 University of Strathclyde. All rights reserved.
"""
from __future__ import division
import sys
import os
import numpy as np
import scipy.stats as stats
import matplotlib
import matplotlib.pyplot as plt
from matplotlib import rc
import warnings
import progressbar as pb
from asymmetries import estimate_bid_functions


## for Palatino and other serif fonts use:
rc('font',**{'family':'sans-serif','sans-serif':['Helvetica']})
rc('text', usetex=True)
matplotlib.rcParams.update({'font.size': 18})

# Ignore RuntimeWarnings for now
warnings.simplefilter("ignore", RuntimeWarning)
# Widgets for ProgressBar
widgets = ['Calculating: ', pb.Percentage(), pb.Bar(marker=pb.RotatingMarker()), ' ', pb.ETA()]


def sample(seed, num, ws):
  # ProgressBar
  pbar = pb.ProgressBar(widgets=widgets, maxval=num).start()
  # PRNG
  prng = np.random.RandomState(seed)
  win_prices = []
  for n in range(num):
    costs = [prng.uniform(0,1), prng.uniform(0,1)]
    reps = [prng.uniform(0,1), prng.uniform(0,1)]
    win_prices_at_n = []
    for w in ws:
      if w != 1.0:
        # Estimate equilibrium bidding strategy function (transformed)
        bids1, values1, bids2, values2 = estimate_bid_functions(w, reps)
        # Calculate Euclidean distance for each value in values1 and costs[0]
        # Get the index and use it to the get the value of the bid
        v1_dist = map(lambda x: np.abs(x - ((1-w)*reps[0] + costs[0]*w)), values1)
        t_bid1 = (bids1[v1_dist.index(min(v1_dist))] - (1-w)*reps[0]) / w
        c_bid1 = w*t_bid1 + (1-w)*reps[0]
        # Repeat for bidder 2
        v2_dist = map(lambda x: np.abs(x - ((1-w)*reps[1] + costs[1]*w)), values2)
        t_bid2 = (bids2[v2_dist.index(min(v2_dist))] - (1-w)*reps[1]) / w
        c_bid2 = w*t_bid2 + (1-w)*reps[1]
      else:
        t_bid1 = (1 + costs[0]) / 2
        c_bid1 = t_bid1
        t_bid2 = (1 + costs[1]) / 2
        c_bid2 = t_bid2
      if c_bid1 < c_bid2:
        win_prices_at_n += [t_bid1]
      elif c_bid1 > c_bid2:
        win_prices_at_n += [t_bid2]
      else:
        win_prices_at_n += [(t_bid1 + t_bid2)/2]
    win_prices += [win_prices_at_n]
    pbar.update(n + 1)
  pbar.finish()
  return win_prices


### Init
N = 1000
ws = np.linspace(0.001, 1.0, 100)
means = []
### Estimate mean using nonparametric methods
# Create the statistic from the sample
for n in range(N):
  means += [list(map(lambda x: sum(x)/len(x), zip(*sample(n, 100, ws))))]
# Average over n replications
avg = list(map(lambda x: sum(x)/N, zip(*means)))
# Calculate sd & 95% confidence intervals
sd = [np.sqrt(sum(map(lambda x: (x-m)**2, tup))/(N-1)) for (tup, m) in zip(zip(*means), avg)]
alpha = 1-0.95
ci = list(map(lambda x: stats.t.ppf(1-alpha/2,N-1)*x/np.sqrt(N), sd))
# Plot the results
# Plot the results
plt.figure()
plt.errorbar(ws, avg, yerr=ci, fmt='ro')
plt.xlabel(r"Price weight, $w$")
plt.ylabel(r"Average price, $\bar{p}(w)$")
plt.ylim([1, 100])
plt.grid()
plt.savefig("expected_prices.pdf")
# Limit the y-range
plt.figure()
plt.errorbar(ws, avg, yerr=ci, fmt='ro')
plt.xlabel(r"Price weight, $w$")
plt.ylabel(r"Average price, $\bar{p}(w)$")
plt.ylim([0, 1])
plt.grid()
plt.savefig("expected_prices_limited.pdf")
