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


def calculate_alpha_bounds(price, blocked, lost, N):
  diff = blocked-lost
  if diff > 0:
    lower = -price * N / diff if (price >= 0 and price < 1) else 0
    upper = (1-price) * N / diff if (price >= 0 and price < 1) else 0
  elif diff < 0:
    lower = (1-price) * N / diff if (price > 0 and price <= 1) else 0
    upper = -price * N / diff if (price > 0 and price <= 1) else 0
  else:
    lower, upper = 0, 0
  return lower, upper


if __name__ == '__main__':
  price = np.random.uniform(0,1)
  print("Init price: {}".format(price))
  num = 10
  for n in range(num):
    blocked = np.random.randint(num+1)
    lost = np.random.randint(num+1)
    print("Blocked, lost: {}, {}".format(blocked, lost))
    lower, upper = calculate_alpha_bounds(price, blocked, lost, num)
    alpha = np.random.uniform(lower, upper)
    price += (blocked-lost) / num * alpha
    print("Current price: {}".format(price))