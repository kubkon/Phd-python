#!/usr/bin/env python2.7
# encoding: utf-8
"""
NumericalToolbox.py

Created by Jakub Konka on 2012-07-23.
Copyright (c) 2012 University of Strathclyde. All rights reserved.
"""

from __future__ import division
import sys
import os
import numpy as np


class NumericalToolbox:
  @classmethod
  def estimate_bid_hat_functions(cls, w, reps, granularity=1000):
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
