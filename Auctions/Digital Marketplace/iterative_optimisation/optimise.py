#!/usr/bin/env python
# encoding: utf-8
"""
optimise.py

Created by Jakub Konka on 2011-05-15.
Copyright (c) 2011 University of Strathclyde. All rights reserved.
"""
from __future__ import division
import sys
import os
import numpy as np
import scipy.integrate as integral
import scipy.stats.mstats as mstats
import scikits.statsmodels as sm
from operator import itemgetter


def simulate(**kwargs):
	try:
		# Parameters
		N = kwargs['N']
		w = kwargs['w']
		pmin = kwargs['pmin']
		pmax = kwargs['pmax']
		update = kwargs['update']
	except KeyError:
		update = 0
	finally:
		# Calculate prices
		costs = [np.random.uniform(pmin, pmax) for i in range(N)] # costs drawn from P~U(pmin,pmax)
		rep = [np.random.uniform(0, 1) for i in range(N)] # reputation drawn from X~U(0,1)
		inv_prices = ((N-1)/pmax / (1-c/pmax)**(N-1) * integral.quad(lambda x,n=N: (1-x/pmax)**(N-2) / x, c, pmax)[0] for c in costs)
		prices = map(lambda x: 1/x, inv_prices) # pricing vector
		# Update
		# lowest_cost = min(enumerate(costs), key=itemgetter(1))[0]
		# prices[lowest_cost] *= (1 - update)
		prices = map(lambda x: x*(1+update), prices)
		# Calculate the winning bids
		bids = map(lambda x,y: w*x + (1-w)*y, prices, rep) # bids
		winner = min(enumerate([bids[n] for n in range(N)]), key=itemgetter(1))[0] # winner
		# return costs[lowest_cost] == costs[winner]
		return min(costs) == costs[winner]
	

def calc_probability(**kwargs):
	iterations = 10000
	results = (simulate(**kwargs) for it in range(iterations))
	num = 0
	for item in results:
		if item == True: num += 1
	return num/iterations
	

def run(n, **kwargs):
	prob_win = []
	# Initial prob of winning
	prob_win.append(calc_probability(**kwargs))
	# Start optimising
	for i in range(n-1):
		kwargs['update'] = 0.05*(i+1)
		prob_win.append(calc_probability(**kwargs))
	return prob_win
	

if __name__ == '__main__':
	result = run(100, N=3, w=0.5, pmin=0, pmax=1)
	gradient = []
	x1 = 0
	for item in result:
		gradient.append(item-x1)
		x1 = item
	del gradient[0]
	print(gradient)

