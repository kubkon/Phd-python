#!/usr/bin/env python
# encoding: utf-8
"""
reserve_price.py

Created by Jakub Konka on 2011-10-11.
Copyright (c) 2011 __MyCompanyName__. All rights reserved.
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


def plot_n_save(types, rep, prices):
	w_range = 1000
	w = np.linspace(0.01, 1, w_range) # price weights
	## Saving & plotting the results
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
	## Figure 1
	# Price evolution vs w
	plt.figure()
	plt.plot(w, prices[0], '-', linewidth=2)
	plt.plot(w, prices[1], '--', linewidth=2)
	plt.xlabel(r"Price weight, $w$")
	plt.ylabel(r"Offered price, $b_i(c_i)$")
	legend = [r"Bidder {0}".format(n+1) for n in range(2)]
	plt.legend(legend, loc='upper right')
	plt.grid()
	plt.savefig("Prices.pdf")
	

def calc_prices(types, rep, reserve):
	w_range = 1000
	w = np.linspace(0.01, 1, w_range) # price weights
	## Prices calculation
	p1 = map(lambda x: x*(1-types[0]**2)/(2*(1-types[0]+(1-x)*rep[0])), w)
	p2 = map(lambda x: x*(1-types[1]**2)/(2*(1-types[1]+(1-x)*rep[1])), w)
	prices = [p1,p2]
	return prices
	
	
def simulate(pmin, pmax):
	types = [np.random.uniform(pmin, pmax) for i in range(2)]
	rep = [np.random.uniform(0, 1) for i in range(2)]
	return [types, rep]


def run():
	reserve = 1
	types, rep = simulate(0, 1)
	prices = calc_prices(types, rep, reserve)
	plot_n_save(types, rep, prices)
	

if __name__ == '__main__':
	run()