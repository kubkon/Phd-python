#!/usr/bin/env python2.7
# encoding: utf-8
"""
std_asymmetries.py

Created by Jakub Konka on 2012-05-20.
Copyright (c) 2012 University of Strathclyde. All rights reserved.
"""
from __future__ import division
import sys
import os
import numpy as np
import scipy.integrate as integrate
import matplotlib.pyplot as plt
  

def estimate_bid_functions(w, reps):
  # Calculate params
  granularity = 10
  v1 = [(1-w)*reps[0], (1-w)*reps[0] + w]
  v2 = [(1-w)*reps[1], (1-w)*reps[1] + w]
  # Check whether nontrivial NE
  condition1 = (v2[0] >= 2*v1[1] - v1[0]) if (v2[0] >= v1[0]) else False
  condition2 = (v1[0] >= 2*v2[1] - v2[0]) if (v2[0] < v1[0]) else False
  if condition1:
    graph_vf1 = np.linspace(v1[0], v1[1], granularity)
    bids1 = graph_vf1
    graph_vf2 = np.linspace(v2[0], v2[1], granularity)
    bids2 = map(lambda x: v1[1], graph_vf1)
  elif condition2:
    graph_vf1 = np.linspace(v1[0], v1[1], granularity)
    bids1 = map(lambda x: v2[1], graph_vf1)
    graph_vf2 = np.linspace(v2[0], v2[1], granularity)
    bids2 = graph_vf2
  else:
    # Bid bounds
    b = [(v1[0] + v2[0]) / 2, (4 * v1[1] * v2[1] - (v1[0] + v2[0])**2) / (4 * (v1[1] - v1[0] + v2[1] - v2[0]))]
    # Constants of integration
    c1 = ((v2[0]-v1[0])**2 + 4*(b[1]-v2[0])*(v1[1]-v1[0])) / (-2*(b[1]-b[0])*(v1[1]-v1[0])) * np.exp((v2[0]-v1[0]) / (2*(b[1]-b[0])))
    c2 = ((v1[0]-v2[0])**2 + 4*(b[1]-v1[0])*(v2[1]-v2[0])) / (-2*(b[1]-b[0])*(v2[1]-v2[0])) * np.exp((v1[0]-v2[0]) / (2*(b[1]-b[0])))
    # Inverse bid functions
    vf1 = lambda x: v1[0] + (v2[0]-v1[0])**2 / (c1*(v2[0]+v1[0]-2*x)*np.exp((v2[0]-v1[0])/(v2[0]+v1[0]-2*x)) + 4*(v2[0]-x))
    vf2 = lambda x: v2[0] + (v1[0]-v2[0])**2 / (c2*(v1[0]+v2[0]-2*x)*np.exp((v1[0]-v2[0])/(v1[0]+v2[0]-2*x)) + 4*(v1[0]-x))
    # Sampling
    bids1, bids2 = np.linspace(b[0], b[1], granularity), np.linspace(b[0], b[1], granularity)
    graph_vf1 = map(vf1, bids1)
    graph_vf2 = map(vf2, bids2)
  return bids1, graph_vf1, bids2, graph_vf2


if __name__ == '__main__':
  ws = np.linspace(0.1, 1.0, 10)
  reps = [0.25, 0.75]
  for w in range(1, 10):
    bids1, values1, bids2, values2 = estimate_bid_functions(w/10, reps)
    print(w, values1)
    print(w, values2)
  