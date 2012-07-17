#!/usr/bin/env python
# encoding: utf-8
"""
winning_prob.py

Created by Jakub Konka on 2011-05-14.
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


def simulate(N, w, pmin, pmax):
	'''This function simulates one stage of the auctioning game among
	the network operators.
	Keyword args:
	N -- number of bidders
	w -- weight of the service agent
	pmin -- minimum price/cost of the service
	pmax -- maximum price/cost of the service
	Returns: True if bidder with lowest cost wins, False otherwise
	'''
	# Calculate prices
	costs = [np.random.uniform(pmin, pmax) for i in range(N)] # costs drawn from P~U(pmin,pmax)
	rep = [np.random.uniform(0, 1) for i in range(N)] # reputation drawn from X~U(0,1)
	# inv_prices = ((N-1)/pmax / (1-c/pmax)**(N-1) * integral.quad(lambda x,n=N: (1-x/pmax)**(N-2) / x, c, pmax)[0] for c in costs)
	# prices = map(lambda x: 1/x, inv_prices) # pricing vector
	prices = [(N-1) / (1 - c)**(N-1) * integral.quad(lambda x, n=N: x * (1-x)**(N-2), c, pmax)[0] for c in costs]
	# Calculate the winning bids
	bids = map(lambda x,y: w*x + (1-w)*y, prices, rep) # bids
	winner = min(enumerate([bids[n] for n in range(N)]), key=itemgetter(1))[0] # winner
	return min(costs) == costs[winner]


def run(n, N, w, pmin, pmax):
	'''This function executes n iterations of the digital marketplace
	auctioning game.
	Keyword args:
	n -- number of iterations
	N -- number of bidders
	w -- weight of the service agent
	pmin -- minimum price/cost of the service
	pmax -- maximum price/cost of the service
	Returns: Estimate of the probability of winning using current bidding strategy
	'''
	# Run the simulation for n times
	results = (simulate(N, w, pmin, pmax) for i in range(n))
	num = 0
	for item in results:
		if item == True: num += 1
	return num/n


if __name__ == '__main__':
	prob = run(1000000, 3, 0.75, 0, 1)
	print("Estimated probabilty of winning: {0}".format(prob))
	
