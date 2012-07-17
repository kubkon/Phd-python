#!/usr/bin/env python2.7
# encoding: utf-8
"""
digital_marketplace.py

Created by Jakub Konka on 2011-04-19.
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
	costs = [np.random.uniform(pmin, pmax) for i in range(N)] # costs drawn from P~U(pmin,pmax)
	# inv_prices = ((N-1)/pmax / (1-c/pmax)**(N-1) * integral.quad(lambda x,n=N: (1-x/pmax)**(N-2) / x, c, pmax)[0] for c in costs)
	# prices = map(lambda x: 1/x, inv_prices) # pricing vector
	prices = [(N-1) / (1 - c)**(N-1) * integral.quad(lambda x, n=N: x * (1-x)**(N-2), c, pmax)[0] for c in costs]
	# Calculation of the winning bid
	rep = [np.random.uniform(0, 1) for i in range(N)] # reputation drawn from X~U(0,1)
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
	# for i in range(N):
	# 	plt.plot(w, bids[i], linewidth=2)
	plt.plot(w, bids[0], '-', linewidth=2)
	plt.plot(w, bids[1], '--', linewidth=2)
	plt.plot(w, bids[2], '-.', linewidth=2)
	plt.ylim([0, pmax])
	plt.xlabel(r"Price weight, $w$")
	plt.ylabel(r"Compound bid, $\beta(b,r)=wb + (1-w)r$")
	legend = [r"Bidder {0}".format(n+1) for n in range(N)]
	plt.legend(legend, loc='upper right')
	plt.grid()
	plt.savefig("Bids.pdf")


if __name__ == '__main__':
	simulate_1_stage(3, 0, 1)

