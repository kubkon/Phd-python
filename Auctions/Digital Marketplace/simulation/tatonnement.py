#!/usr/bin/env python2.7
# encoding: utf-8
"""
tatonnement.py

Created by Jakub Konka on 2012-07-19.
Copyright (c) 2012 University of Strathclyde. All rights reserved.
"""
from __future__ import division
import sys
import os
import numpy as np
import matplotlib
import matplotlib.pyplot as plt
from matplotlib import rc
import warnings
import progressbar as pb


# Set up LaTeX fonts
rc('font',**{'family':'sans-serif','sans-serif':['Helvetica']})
rc('text', usetex=True)
matplotlib.rcParams.update({'font.size': 18})

# Ignore RuntimeWarnings for now
warnings.simplefilter("ignore", RuntimeWarning)
# Widgets for ProgressBar
widgets = ['Calculating: ', pb.Percentage(), pb.Bar(marker=pb.RotatingMarker()), ' ', pb.ETA()]


def estimate_bid_hat_functions(w, reps, granularity=1000):
  # Calculate params
  v1 = [(1-w)*reps[0], (1-w)*reps[0] + w]
  v2 = [(1-w)*reps[1], (1-w)*reps[1] + w]
  # Check whether nontrivial NE
  if (v2[1] >= v1[1]):
    if (v1[1] <= 2*v2[0] - v2[1]):
      graph_vf1 = np.linspace(v1[0], v1[1], granularity)
      bids1 = map(lambda x: v2[0], graph_vf1)
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
      graph_vf1 = map(vf1, bids1)
      graph_vf2 = map(vf2, bids2)
  else:
    if (v2[1] <= 2*v1[0] - v1[1]):
      graph_vf1 = np.linspace(v1[0], v1[1], granularity)
      bids1 = graph_vf1
      graph_vf2 = np.linspace(v2[0], v2[1], granularity)
      bids2 = map(lambda x: v1[0], graph_vf2)
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
      graph_vf1 = map(vf1, bids1)
      graph_vf2 = map(vf2, bids2)
  return bids1, graph_vf1, bids2, graph_vf2


def estimate_bids(w, costs, reps):
  bid1, bid2 = 0, 0
  if w != 1.0:
    # Estimate equilibrium bidding strategy functions (bids-hat)
    bids_hat1, costs_hat1, bids_hat2, costs_hat2 = estimate_bid_hat_functions(w, reps)
    # Get bid and compound bid of bidder 1
    dist = map(lambda x: np.abs(x - ((1-w)*reps[0] + costs[0]*w)), costs_hat1)
    bid1 = (bids_hat1[dist.index(min(dist))] - (1-w)*reps[0]) / w
    # Get bid and compound bid of bidder 2
    dist = map(lambda x: np.abs(x - ((1-w)*reps[1] + costs[1]*w)), costs_hat2)
    bid2 = (bids_hat2[dist.index(min(dist))] - (1-w)*reps[1]) / w
  else:
    # Get bid of bidder 1
    bid1 = (1 + costs[0]) / 2
    # Get bid of bidder 2
    bid2 = (1 + costs[1]) / 2
  return [bid1, bid2]


def simulate(runs, N, w):
  pbar = pb.ProgressBar(widgets=widgets, maxval=runs).start()
  ### Simulation output
  prices = []
  winners = []
  ### First run
  blocked = [0, 0]
  costs = [np.random.uniform(0,1), np.random.uniform(0,1)]
  reps = [0.5, 0.5]
  bids = estimate_bids(w, costs, reps)
  # Select the winner and update statistics
  c_bids = map(lambda b,r: w*b + (1-w)*r, bids, reps)
  if c_bids[0] < c_bids[1]:
    # Bidder 1 wins
    prices += [bids[0]]
    winners += [0]
  elif c_bids[0] > c_bids[1]:
    # Bidder 2 wins
    prices += [bids[1]]
    winners += [1]
  else:
    # Tie
    winner = np.random.randint(2)
    prices += [bids[winner]]
    winners += [winner]
  pbar.update(1)
  ### Consecutive runs
  for r in range(1, runs):
    # Calculate cost update for bidder 1
    lost = winners[len(winners)-N:].count(0)
    lower, upper = calculate_alpha_bounds(costs[0], blocked[0], lost, N)
    alpha = np.random.uniform(lower, upper)
    costs[0] += (blocked[0] - lost) / N * alpha
    # Calculate cost update for bidder 2
    lost = winners[len(winners)-N:].count(1)
    lower, upper = calculate_alpha_bounds(costs[1], blocked[1], lost, N)
    alpha = np.random.uniform(lower, upper)
    costs[1] += (blocked[1] - lost) / N * alpha
    # Update reps for both bidders
    reps = [np.random.uniform(0,1), np.random.uniform(0,1)]
    # Calculate bids, select the winner, and update statistics
    bids = estimate_bids(w, costs, reps)
    c_bids = map(lambda b,r: w*b + (1-w)*r, bids, reps)
    if c_bids[0] < c_bids[1]:
      # Bidder 1 wins
      prices += [bids[0]]
      winners += [0]
    elif c_bids[0] > c_bids[1]:
      # Bidder 2 wins
      prices += [bids[1]]
      winners += [1]
    else:
      # Tie
      winner = np.random.randint(2)
      prices += [bids[winner]]
      winners += [winner]
    pbar.update(r+1)
  pbar.finish()
  ### Plot the output
  x = [r+1 for r in range(runs)]
  plt.figure()
  plt.plot(x, prices, '.')
  plt.xlabel(r"Simulation run")
  plt.ylabel(r"Price")
  plt.grid()
  plt.savefig("prices.pdf")
  plt.figure()
  plt.plot(x, winners, '*')
  plt.xlabel(r"Simulation run")
  plt.ylabel(r"Winning bidder")
  plt.grid()
  plt.savefig("winners.pdf")


def calculate_alpha_bounds(cost, blocked, lost, N):
  diff = blocked-lost
  if diff > 0:
    lower = -cost * N / diff if (cost >= 0 and cost < 1) else 0
    upper = (1-cost) * N / diff if (cost >= 0 and cost < 1) else 0
  elif diff < 0:
    lower = (1-cost) * N / diff if (cost > 0 and cost <= 1) else 0
    upper = -cost * N / diff if (cost > 0 and cost <= 1) else 0
  else:
    lower, upper = 0, 0
  return lower, upper


if __name__ == '__main__':
  # Get command line args
  runs = int(sys.argv[1])
  N = int(sys.argv[2])
  w = float(sys.argv[3])
  # Simulate
  simulate(runs, N, w)