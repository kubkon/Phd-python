#!/usr/bin/env python2.7
# encoding: utf-8
"""
std_asymmetries.py

Created by Jakub Konka on 2012-05-31.
Copyright (c) 2012 University of Strathclyde. All rights reserved.
"""

from __future__ import division
import sys
import os
import math
import numpy as np
import scipy.integrate as integrate
import matplotlib.pyplot as plt


def main():
  # Params
  v1 = [0.125, 0.625]
  v2 = [0.375, 0.875]
  # Bid bounds
  b = [(4 * v1[0] * v2[0] - (v1[1] + v2[1])**2) / (4 * (v1[0] - v1[1] + v2[0] - v2[1])), (v1[1] + v2[1]) / 2]
  print(b)
  # Constants of integration
  c1 = ((v2[1]-v1[1])**2 + 4*(b[0]-v2[1])*(v1[0]-v1[1])) / (-2*(b[0]-b[1])*(v1[0]-v1[1])) * math.exp((v2[1]-v1[1]) / (2*(b[0]-b[1])))
  c2 = ((v1[1]-v2[1])**2 + 4*(b[0]-v1[1])*(v2[0]-v2[1])) / (-2*(b[0]-b[1])*(v2[0]-v2[1])) * math.exp((v1[1]-v2[1]) / (2*(b[0]-b[1])))
  print(c1)
  print(c2)
  # Inverse bid functions
  vf1 = lambda x: v1[1] + (v2[1]-v1[1])**2 / (c1*(v2[1]+v1[1]-2*x)*math.exp((v2[1]-v1[1])/(v2[1]+v1[1]-2*x)) + 4*(v2[1]-x))
  vf2 = lambda x: v2[1] + (v1[1]-v2[1])**2 / (c2*(v1[1]+v2[1]-2*x)*math.exp((v1[1]-v2[1])/(v1[1]+v2[1]-2*x)) + 4*(v1[1]-x))
  # Sampling & plotting
  bids = np.linspace(b[0], b[1], 100)
  graph_vf1 = map(vf1, bids)
  graph_vf2 = map(vf2, bids)
  plt.figure()
  plt.plot(bids, graph_vf1, '-', linewidth=2)
  plt.plot(bids, graph_vf2, '--', linewidth=2)
  plt.xlabel(r"Bid, $b$")
  plt.ylabel(r"Value, $v(b)$")
  plt.grid()
  plt.figure()
  plt.plot(graph_vf1, bids, '-', linewidth=2)
  plt.plot(graph_vf2, bids, '--', linewidth=2)
  plt.xlabel(r"Value, $v$")
  plt.ylabel(r"Bid, $b(v)$")
  plt.grid()
  plt.show()


if __name__ == '__main__':
	main()

