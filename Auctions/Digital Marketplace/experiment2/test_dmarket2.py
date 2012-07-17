#!/usr/bin/env python2.7
# encoding: utf-8
"""
test_dmarket2.py

Created by Jakub Konka on 2011-05-09.
Copyright (c) 2011 University of Strathclyde. All rights reserved.
"""
from __future__ import division
import sys
import os
import numpy as np
import scipy.integrate as integral
import scipy.stats.mstats as mstats
import scikits.statsmodels as sm
import matplotlib.pyplot as plt
from operator import itemgetter
from matplotlib import rc


rc('font',**{'family':'sans-serif','sans-serif':['Helvetica']})
## for Palatino and other serif fonts use:
#rc('font',**{'family':'serif','serif':['Palatino']))
rc('text', usetex=True)


def simulate_1_stage(N, pmin, pmax):
	'''This function simulates one stage of the auctioning game among
	the network operators.
	Keyword args:
	N -- number of bidders
	pmin -- minimum price/cost of the service
	pmax -- maximum price/cost of the service
	Returns: intersection (if exists) of the winning bidders for price weight->1
	'''
	# Service agent weights
	w_1 = np.linspace(0, 0.6, 600)
	w_2 = np.linspace(0.6, 1, 400)
	# Calculate prices
	costs = [np.random.uniform(pmin, pmax) for i in range(N)] # costs drawn from P~U(pmin,pmax)
	inv_prices = ((N-1)/pmax / (1-c/pmax)**(N-1) * integral.quad(lambda x,n=N: (1-x/pmax)**(N-2) / x, c, pmax)[0] for c in costs)
	prices = map(lambda x: 1/x, inv_prices) # pricing vector
	# Calculate the winning bids
	rep = [np.random.uniform(0, 1) for i in range(N)] # reputation drawn from R~U(0,1)
	bids_1 = map(lambda x,y: w_1*x + (1-w_1)*y, costs, rep) # bids
	bids_2 = map(lambda x,y: w_2*x + (1-w_2)*y, prices, rep)
	# Plot figure
	plt.figure()
	b1_a = plt.plot(w_1, bids_1[0], 'r', linewidth=2)
	b1_b = plt.plot(w_2, bids_2[0], 'r', linewidth=2)
	b2_a = plt.plot(w_1, bids_1[1], 'g', linewidth=2)
	b2_b = plt.plot(w_2, bids_2[1], 'g', linewidth=2)
	b3_a = plt.plot(w_1, bids_1[2], 'b', linewidth=2)
	b3_b = plt.plot(w_2, bids_2[2], 'b', linewidth=2)
	plt.ylim([0, pmax])
	plt.xlabel(r"Price weight, $w$")
	plt.ylabel(r"Bid, $w\cdot p + (1-w)\cdot r$")
	plt.legend((b1_a, b2_a, b3_a), (r"Bidder 1", r"Bidder 2", r"Bidder 3"), loc='upper left')
	plt.grid()
	plt.savefig("Bids.pdf")


if __name__ == '__main__':
	simulate_1_stage(3, 0 ,1)

