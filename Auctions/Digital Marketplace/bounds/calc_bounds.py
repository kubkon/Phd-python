#!/usr/bin/env python2.7
# encoding: utf-8
"""
calc_bounds.py

Created by Jakub Konka on 2011-07-19.
Copyright (c) 2011 University of Strathclyde. All rights reserved.
"""
from __future__ import division
import sys
import os
import numpy as np
import scipy.integrate as integral
import matplotlib.pyplot as plt
from operator import itemgetter
from matplotlib import rc


rc('font', **{'family':'sans-serif','sans-serif':['Helvetica']})
## for Palatino and other serif fonts use:
#rc('font',**{'family':'serif','serif':['Palatino']))
rc('text', usetex=True)


def simulate(n, w):
	costs = [[np.random.uniform(0, 1), np.random.uniform(0, 1)] for i in range(n)]
	reps = [[np.random.uniform(0, 1), np.random.uniform(0, 1)] for i in range(n)]
	max_bid = map(lambda c, r: max(c) + (1-w)/w * (max(r) - min(r)), costs, reps)
	print(max(max_bid))


if __name__ == '__main__':
	n = 1000000
	w = 0.001
	simulate(n, w)
