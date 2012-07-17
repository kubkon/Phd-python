#!/usr/bin/env python2.7
# encoding: utf-8
"""
max_digital_marketplace.py

Created by Jakub Konka on 2011-06-08.
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
	w = np.linspace(0.01, 1, w_range) # Price weights
	# Prices calculation
	types = [np.random.uniform(pmin, pmax) for i in range(N)] # types drawn from P~U(pmin,pmax)
	rep = [np.random.uniform(0, 1) for i in range(N)] # reputation drawn from X~U(0,1)
	inv_fpa = ((N-1)/pmax / (1-c/pmax)**(N-1) * integral.quad(lambda x,n=N: (1-x/pmax)**(N-2) / x, c, pmax)[0] for c in types)
	fpa = map(lambda x: 1/x, inv_fpa) # fpa prices
	#prices = [map(lambda x: max([fpa[i], 0.5 + (1-x)/x * (0.5-rep[i])]), w) for i in range(N)] # pricing vector
	exp_tmin = integral.quad(lambda x, n=N: x*(1-x)**(n-2), pmin, pmax)
	exp_rmin = integral.quad(lambda x, n=N: x*(1-x)**(n-2), pmin, pmax)
	prices = [map(lambda x: max([types[i], (N-1)*exp_tmin[0] + (1-x)/x * ((N-1)*exp_rmin[0]-rep[i])]), w) for i in range(N)] # pricing vector
	# Calculation of the winning bid
	bids = map(lambda x,y: w*x + (1-w)*y, prices, rep) # bids
	# Saving & plotting the results
	# Write values=(costs, prices, reputations) to file
	try:
		f = open('values.txt', 'w')
		types_to_str = 'Types: {0}\n'.format(types)
		rep_to_str = 'Reps: {0}\n'.format(rep)
		val_to_str = types_to_str + rep_to_str
		f.write(val_to_str)
		f.close()
	except IOError:
		print('Cannot open file for writing!')
	# Figure 1
	# Price evolution vs w
	plt.figure()
	for i in range(N):
		plt.plot(w, prices[i], linewidth=2)
	ticks = [repr(n+1) for n in range(N)]
	plt.xlabel(r"Price weight, $w_{price}$")
	plt.ylabel(r"Offered price, $p_i$")
	legend = [r"Bidder {0}".format(n+1) for n in range(N)]
	plt.legend(legend, loc='upper right')
	plt.grid()
	plt.savefig("Prices.pdf")
	# Figure 2
	# Bids evolution vs w
	plt.figure()
	for i in range(N):
		plt.plot(w, bids[i], linewidth=2)
	# plt.plot(w, bids[0], '-', linewidth=2)
	# plt.plot(w, bids[1], '--', linewidth=2)
	# plt.plot(w, bids[2], '-.', linewidth=2)
	plt.ylim([0, pmax])
	plt.xlabel(r"Price weight, $w_{price}$")
	plt.ylabel(r"Compound bid, $b(p,r)=w_{price}\cdot p + (1-w_{price})\cdot r$")
	legend = [r"Bidder {0}".format(n+1) for n in range(N)]
	plt.legend(legend, loc='upper left')
	plt.grid()
	plt.savefig("Bids.pdf")


if __name__ == '__main__':
	simulate_1_stage(2, 0, 1)

