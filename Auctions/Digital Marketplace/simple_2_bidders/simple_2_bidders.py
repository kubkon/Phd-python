#!/usr/bin/env python
# encoding: utf-8
"""
simple_2_bidders.py

Created by Jakub Konka on 2011-08-26.
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


def plot_n_save(types, rep, prices, bids, mode):
	w_range = 1000
	w = np.linspace(0.01, 1, w_range) # price weights
	## Saving & plotting the results
	# Write values=(costs, prices, reputations) to file
	try:
		f = open('values_' + mode + '.txt', 'w')
		types_to_str = 'Types: {0}\n'.format(types)
		rep_to_str = 'Reps: {0}\n'.format(rep)
		val_to_str = types_to_str + rep_to_str
		f.write(val_to_str)
		f.close()
	except IOError:
		print('Cannot open file for writing!')
	## Figure 1
	# Price evolution vs w
	plt.figure()
	plt.plot(w, prices[0], '-', linewidth=2)
	plt.plot(w, prices[1], '--', linewidth=2)
	plt.xlabel(r"Price weight, $w$")
	plt.ylabel(r"Offered price, $b_i(c_i)$")
	#plt.ylim([0, 5])
	legend = [r"Bidder {0}".format(n+1) for n in range(2)]
	plt.legend(legend, loc='upper right')
	plt.grid()
	plt.savefig("Prices_" + mode + ".pdf")
	## Figure 2
	# Bids evolution vs w
	plt.figure()
	plt.plot(w, bids[0], '-', linewidth=2)
	plt.plot(w, bids[1], '--', linewidth=2)
	plt.ylim([0, 1])
	plt.xlabel(r"Price weight, $w$")
	plt.ylabel(r"Compound bid, $\beta(b_i(c_i),r_i)=wb_i(c_i) + (1-w)r_i$")
	legend = [r"Bidder {0}".format(n+1) for n in range(2)]
	plt.legend(legend, loc='upper left')
	plt.grid()
	plt.savefig("Bids_" + mode + ".pdf")
	

def calc_unconstrained(types, rep):
	w_range = 1000
	w = np.linspace(0.01, 1, w_range) # price weights
	## Prices calculation
	p1 = map(lambda x: 0.5 + 0.5*types[0] - (1-x)*(rep[0]-rep[1])/(3*x), w)
	p2 = map(lambda x: 0.5 + 0.5*types[1] - (1-x)*(rep[1]-rep[0])/(3*x), w)
	prices = [p1, p2]
	## Calculation of the winning bid
	bids = map(lambda x,y: w*x + (1-w)*y, prices, rep)
	return [prices, bids]
	

def calc_constrained(types, rep):
	w_range = 1000
	w = np.linspace(0.01, 1, w_range) # price weights
	## Prices calculation
	p1 = map(lambda x: max(types[0], 0.5 + 0.5*types[0] - (1-x)*(rep[0]-rep[1])/(3*x)), w)
	p2 = map(lambda x: max(types[1], 0.5 + 0.5*types[1] - (1-x)*(rep[1]-rep[0])/(3*x)), w)
	prices = [p1, p2]
	## Calculation of the winning bid
	bids = map(lambda x,y: w*x + (1-w)*y, prices, rep)
	return [prices, bids]
	
	
def simulate(pmin, pmax):
	types = [np.random.uniform(pmin, pmax) for i in range(2)]
	rep = [np.random.uniform(0, 1) for i in range(2)]
	return [types, rep]


def run():
	types, rep = simulate(0, 1)
	prices_uc, bids_uc = calc_unconstrained(types, rep)
	prices_c, bids_c = calc_constrained(types, rep)
	plot_n_save(types, rep, prices_uc, bids_uc, "uc")
	plot_n_save(types, rep, prices_c, bids_c, "c")
	

if __name__ == '__main__':
	run()