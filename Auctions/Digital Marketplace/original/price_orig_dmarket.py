#!/usr/bin/env python2.7
# encoding: utf-8
"""
price_dmarket.py

Created by Jakub Konka on 2011-05-09.
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


rc('font',**{'family':'sans-serif','sans-serif':['Helvetica']})
## for Palatino and other serif fonts use:
#rc('font',**{'family':'serif','serif':['Palatino']))
rc('text', usetex=True)


def simulate_1_stage(N, pmin, pmax):
	'''This function simulates one stage game between network operators
	in the digital marketplace setting.
	Keyword args:
	N -- number of bidders
	pmin -- minimum price/cost of the service
	pmax -- maximum price/cost of the serivce
	'''
	w_range = 1000
	w = np.linspace(0, 1, w_range) # Price weights
	# Prices calculation
	max_n = 100 # maximum number of contracts that can be supported by the bidder
	beta = 1 # parameter characterising price evolution
	r = 50 # number of contract tenders already underway minus the current tender
	costs = [np.random.uniform(pmin, pmax) for i in range(N)] # costs drawn from P~U(pmin,pmax)
	rep = [np.random.uniform(0, 1) for i in range(N)] # reputation drawn from X~U(0,1)
	prices = map(lambda x: x + (np.exp(((r-1)/(max_n-1))**beta) - 1) / (np.exp(beta) - 1) * (pmax-x), costs) # pricing vector
	# Calculation of the winning bid
	bids = map(lambda x,y: w*x + (1-w)*y, prices, rep) # bids
	winners = [min(enumerate([bids[n][i] for n in range(N)]), key=itemgetter(1))[0] for i in range(w_range)] # winners
	# Saving & plotting the results
	# Write values=(costs, prices, reputations) to file
	try:
		f = open('values.txt', 'w')
		val_to_str = 'Costs: {0}\nPrices: {1}\nReps: {2}'.format(costs, prices, rep)
		f.write(val_to_str)
		f.close()
	except IOError:
		print('Cannot open file for writing!')
	# Figure 1
	plt.figure()
	plt.plot(w, winners, 'bo')
	plt.ylim([-1, N])
	ticks = [repr(n+1) for n in range(N)]
	plt.yticks(np.arange(N), ticks)
	plt.xlim([0, 1])
	plt.xlabel(r"Price weight, $w$")
	plt.ylabel(r"Winner, $N$")
	plt.grid()
	plt.savefig("Winners.pdf")
	# Figure 2
	plt.figure()
	for i in range(N):
		plt.plot(w, bids[i], linewidth=2)
	plt.ylim([0, pmax])
	plt.xlabel(r"Price weight, $w$")
	plt.ylabel(r"Bid, $w\cdot p + (1-w)\cdot r$")
	legend = [r"Bidder {0}".format(n+1) for n in range(N)]
	plt.legend(legend, loc='upper left')
	plt.grid()
	plt.savefig("Bids.pdf")


if __name__ == '__main__':
	simulate_1_stage(3, 0, 1)

