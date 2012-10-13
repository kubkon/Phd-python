#!/usr/bin/env python
# encoding: utf-8
"""
expected_prices.py

Created by Jakub Konka on 2012-10-13.
Copyright (c) 2012 University of Strathclyde. All rights reserved.
"""
import argparse
import matplotlib
import matplotlib.pyplot as plt
from matplotlib import rc
import numpy as np
import os
import progressbar as pb
import scipy.stats as stats
import warnings

## for Palatino and other serif fonts use:
rc('font',**{'family':'sans-serif','sans-serif':['Helvetica']})
rc('text', usetex=True)
matplotlib.rcParams.update({'font.size': 18})

# Ignore RuntimeWarnings for now
warnings.simplefilter("ignore", RuntimeWarning)
# Widgets for ProgressBar
widgets = ['Calculating: ', pb.Percentage(), pb.Bar(marker=pb.RotatingMarker()), ' ', pb.ETA()]

def estimate_bid_functions(w, reps, granularity=1000):
  # Calculate params
  v1 = [(1-w)*reps[0], (1-w)*reps[0] + w]
  v2 = [(1-w)*reps[1], (1-w)*reps[1] + w]
  # Check whether nontrivial NE
  if (v2[1] >= v1[1]):
    if (v1[1] <= 2*v2[0] - v2[1]):
      graph_vf1 = np.linspace(v1[0], v1[1], granularity)
      bids1 = list(map(lambda x: v2[0], graph_vf1))
      graph_vf2 = np.linspace(v2[0], v2[1], granularity)
      bids2 = graph_vf2
    else:
      # Bid bounds
      b = [(4 * v1[0] * v2[0] - (v1[1] + v2[1])**2) / (4 * (v1[0] - v1[1] + v2[0] - v2[1])), (v1[1] + v2[1]) / 2]
      # Constants of integration
      c1 = ((v2[1]-v1[1])**2 + 4*(b[0]-v2[1])*(v1[0]-v1[1])) / (-2*(b[0]-b[1])*(v1[0]-v1[1])) * np.exp((v2[1]-v1[1]) / (2*(b[0]-b[1])))
      c2 = ((v1[1]-v2[1])**2 + 4*(b[0]-v1[1])*(v2[0]-v2[1])) / (-2*(b[0]-b[1])*(v2[0]-v2[1])) * np.exp((v1[1]-v2[1]) / (2*(b[0]-b[1])))
      # Inverse bid functions
      vf1 = lambda x: v1[1] + (v2[1]-v1[1])**2 / (c1*(v2[1]+v1[1]-2*x)*np.exp((v2[1]-v1[1])/(v2[1]+v1[1]-2*x)) + 4*(v2[1]-x))
      vf2 = lambda x: v2[1] + (v1[1]-v2[1])**2 / (c2*(v1[1]+v2[1]-2*x)*np.exp((v1[1]-v2[1])/(v1[1]+v2[1]-2*x)) + 4*(v1[1]-x)) \
            if x <= b[1] else x
      # Sampling
      bids1 = np.linspace(b[0], b[1], granularity)
      bids2 = np.linspace(b[0], v2[1], granularity)
      graph_vf1 = list(map(vf1, bids1))
      graph_vf2 = list(map(vf2, bids2))
  else:
    if (v2[1] <= 2*v1[0] - v1[1]):
      graph_vf1 = np.linspace(v1[0], v1[1], granularity)
      bids1 = graph_vf1
      graph_vf2 = np.linspace(v2[0], v2[1], granularity)
      bids2 = list(map(lambda x: v1[0], graph_vf2))
    else:
      # Bid bounds
      b = [(4 * v1[0] * v2[0] - (v1[1] + v2[1])**2) / (4 * (v1[0] - v1[1] + v2[0] - v2[1])), (v1[1] + v2[1]) / 2]
      # Constants of integration
      c1 = ((v2[1]-v1[1])**2 + 4*(b[0]-v2[1])*(v1[0]-v1[1])) / (-2*(b[0]-b[1])*(v1[0]-v1[1])) * np.exp((v2[1]-v1[1]) / (2*(b[0]-b[1])))
      c2 = ((v1[1]-v2[1])**2 + 4*(b[0]-v1[1])*(v2[0]-v2[1])) / (-2*(b[0]-b[1])*(v2[0]-v2[1])) * np.exp((v1[1]-v2[1]) / (2*(b[0]-b[1])))
      # Inverse bid functions
      vf1 = lambda x: v1[1] + (v2[1]-v1[1])**2 / (c1*(v2[1]+v1[1]-2*x)*np.exp((v2[1]-v1[1])/(v2[1]+v1[1]-2*x)) + 4*(v2[1]-x)) \
            if x <= b[1] else x
      vf2 = lambda x: v2[1] + (v1[1]-v2[1])**2 / (c2*(v1[1]+v2[1]-2*x)*np.exp((v1[1]-v2[1])/(v1[1]+v2[1]-2*x)) + 4*(v1[1]-x))
      # Sampling
      bids1 = np.linspace(b[0], v1[1], granularity)
      bids2 = np.linspace(b[0], b[1], granularity)
      graph_vf1 = list(map(vf1, bids1))
      graph_vf2 = list(map(vf2, bids2))
  return bids1, graph_vf1, bids2, graph_vf2

def sample(prng, reps, ws):
  win_prices = []
  costs = [prng.uniform(0,1), prng.uniform(0,1)]
  for w in ws:
    if w != 1.0 and reps[0] != reps[1]:
      # Estimate equilibrium bidding strategy function (transformed)
      bids1, values1, bids2, values2 = estimate_bid_functions(w, reps)
      # Calculate Euclidean distance for each value in values1 and costs[0]
      # Get the index and use it to the get the value of the bid
      v1_dist = list(map(lambda x: np.abs(x - ((1-w)*reps[0] + costs[0]*w)), values1))
      t_bid1 = (bids1[v1_dist.index(min(v1_dist))] - (1-w)*reps[0]) / w
      c_bid1 = w*t_bid1 + (1-w)*reps[0]
      # Repeat for bidder 2
      v2_dist = list(map(lambda x: np.abs(x - ((1-w)*reps[1] + costs[1]*w)), values2))
      t_bid2 = (bids2[v2_dist.index(min(v2_dist))] - (1-w)*reps[1]) / w
      c_bid2 = w*t_bid2 + (1-w)*reps[1]
    else:
      t_bid1 = (1 + costs[0]) / 2
      c_bid1 = t_bid1
      t_bid2 = (1 + costs[1]) / 2
      c_bid2 = t_bid2
    if c_bid1 < c_bid2:
      win_prices += [t_bid1]
    elif c_bid1 > c_bid2:
      win_prices += [t_bid2]
    else:
      bid = t_bid1 if prng.randint(2) == 0 else t_bid2
      win_prices += [bid]
  return win_prices

### Parse command line arguments
parser = argparse.ArgumentParser(description="Expected prices in one-shot DM auction")
parser.add_argument('N', metavar='N',
                    type=int, help='number of replications')
parser.add_argument('reputations', metavar='reputations',
                    help='input reputation list; comma separated')
parser.add_argument('--save_dir', dest='save_dir', default='out',
                    help='output directory')
parser.add_argument('--seed', dest='seed', default=0,
                    type=int, help='base for seed values')
parser.add_argument('--confidence', dest='confidence', default=0.95,
                    type=float, help='confidence value')
args = parser.parse_args()
N = args.N
reps = list(map(lambda x: float(x), args.reputations.split(',')))
save_dir = args.save_dir
seed = args.seed
confidence = args.confidence

### Init
ws = np.linspace(0.001, 1.0, 100)
prng = np.random.RandomState(seed)
# Create dir for saving if doesn't exist already
if not os.path.exists(save_dir):
  os.makedirs(save_dir)

### Estimate means
print("Replicating {} times...".format(N))
# ProgressBar
pbar = pb.ProgressBar(widgets=widgets, maxval=N).start()
# Run for N times (different seed)
prices = []
for n in range(N):
  prices += [sample(prng, reps, ws)]
  pbar.update(n + 1)
pbar.finish()
print("Calculating the means and confidence intervals...")
# Average over n replications
means = list(map(lambda x: sum(x)/N, zip(*prices)))
# Calculate sd & 95% confidence intervals
sd = [np.sqrt(sum(map(lambda x: (x-m)**2, tup))/(N-1)) for (tup, m) in zip(zip(*prices), means)]
alpha = 1-confidence
ci = list(map(lambda x: stats.t.ppf(1-alpha/2,N-1)*x/np.sqrt(N), sd))

### Plot
print("Plotting the results...")
# Plot the results
plt.figure()
plt.errorbar(ws, means, yerr=ci, fmt='ro')
plt.xlabel(r"Price weight, $w$")
plt.ylabel(r"Average price, $\bar{p}(w)$")
plt.ylim([1, 100])
plt.grid()
plt.savefig(save_dir + "/expected_prices_{}_{}.pdf".format(*reps))
# Limit the y-range
plt.figure()
plt.errorbar(ws, means, yerr=ci, fmt='ro')
plt.xlabel(r"Price weight, $w$")
plt.ylabel(r"Average price, $\bar{p}(w)$")
plt.ylim([0, 1])
plt.grid()
plt.savefig(save_dir + "/expected_prices_{}_{}_limited.pdf".format(*reps))
